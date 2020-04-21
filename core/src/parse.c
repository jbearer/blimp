////////////////////////////////////////////////////////////////////////////////
// The bl:mp Parser
//
// This module contains a recursive-descent implementation of a parser for the
// bl:mp expression grammar:
//
//  <expr> ::= <symbol>
//          |  '{' <expr> '|' <expr> '}'
//          | <expr> <expr>
//          | 'bind' <expr> <expr> <expr>
//          |  <expr> ';' <expr>
//
//  <symbol> ::= <operator-char>+<identifier-char>*
//            |  <operator-char>*<identifier-char>+
//
// This grammar is a fully adequate description of the bl:mp AST, in that any
// valid bl:mp parse tree is produced by this grammar. However, the grammar as
// given is not a sufficient description of the bl:mp concrete syntax, because
// it is ambiguous: the associativity and precedence of the operators such as
// "sequence" (;), "bind", and "send" (<expr> <expr>) are not specified.
//
// To make the grammar unambiguous, this implementation adds two additional
// non-terminals: `term`, which represents "atomic" expressions (e.g. literals
// and parenthesized compound expressions), and `stmt`, which represents a
// single expression in a sequence. We also add an additional production to
// allow parenthesized expressions.
//
//  <expr> ::= <stmt> ';' <expr>
//          |  <stmt>
//
//  <stmt> ::= <stmt> <term>
//          |  <term>
//
//  <term> ::= <symbol>
//          |  '{' <expr> '|' <expr> '}'
//          |  '(' <expr> ')'
//          | 'bind' <term> <term> <term>
//
// With this extended grammar, it is possible to see that:
//  * Precedence order is "bind", "send", "sequence"
//  * Send is left-associative (this is required by bl:mp semantics)
//  * Sequence is right-associative (this is arbitrary, as ; is semantically
//    associativity, but right-associativity is easier to parse)
//
// The implementation of the parser is divided into three sub-components:
//  * I/O streams
//  * Lexing
//  * Parsing
//
// The Stream interface allows the rest of the parser to be polymorphic in the
// source of the input. That is, we can parse a string just as easily as we can
// parse from a file by writing the appropriate implementations of the Stream
// interface (StringStream and FileStream are provided in this file, but the
// user can parse from custom Stream implementations as well).
//
// The lexer implements its own stream-based API. It is not a separate pass over
// the input, but rather a stream that traverses the input stream and feeds
// tokens to the parser simultaneously.
//
// The parser is, for the most part, straight-forward. It requests tokens from
// the lexer one at a time and converts the token stream into an Expr using a
// recursive-descent algorithm. There is, however, one production that requires
// special handling: the left recursion in the <stmt> production for "send"
// (<stmt> <term>) cannot be handled by naive recursive-descent. We handle this
// case with an ad hoc algorithm which parses as many <term>s as it can from
// left to right, folding them into a "send" <expr> as it goes.
//

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <string.h>

#include "internal/blimp.h"
#include "internal/error.h"
#include "internal/expr.h"

////////////////////////////////////////////////////////////////////////////////
// I/O
//

typedef struct {
    Stream base;
    Blimp *blimp;
    SourceLoc loc;
    FILE *f;
} FileStream;

static Status FileStream_Next(Stream *stream, int *c)
{
    FileStream *self = (FileStream *)stream;

    *c = fgetc(self->f);
    if ((errno = ferror(self->f)) != 0) {
        return ErrorAt(self->blimp, self->loc, BLIMP_IO_ERROR,
            "I/O error: %s", strerror(errno));
    }

    if (*c == '\n') {
        self->loc.row++;
        self->loc.col = 0;
    } else {
        self->loc.col++;
    }

    return BLIMP_OK;
}

static SourceLoc FileStream_Location(Stream *stream)
{
    return ((FileStream *)stream)->loc;
}

static void FileStream_Close(Stream *stream)
{
    FileStream *self = (FileStream *)stream;
    fclose(self->f);
    Free(self->blimp, &self);
}

static Status FileStream_New(Blimp *blimp, const char *path, FileStream **self)
{
     TRY(Malloc(blimp, sizeof(FileStream), self));
     **self = (FileStream) {
        .base = {
            .Next     = FileStream_Next,
            .Location = FileStream_Location,
            .Close    = FileStream_Close,
        },
        .blimp = blimp,
        .loc = {
            .file = path,
            .row  = 0,
            .col  = 0,
        },
        .f = fopen(path, "r"),
     };
     if ((*self)->f == NULL) {
        return ErrorMsg(
            blimp, BLIMP_IO_ERROR, "cannot open %s: %s", path, strerror(errno));
     }

     return BLIMP_OK;
}

typedef struct {
    Stream base;
    const char *str;
    size_t row;
    size_t col;
} StringStream;

static Status StringStream_Next(Stream *stream, int *c)
{
    StringStream *self = (StringStream *)stream;
    *c = *self->str++;
    if (!*c) {
        *c = EOF;
    }
    if (*c == '\n') {
        self->row++;
        self->col = 0;
    } else {
        self->col++;
    }

    return BLIMP_OK;
}

static SourceLoc StringStream_Location(Stream *stream)
{
    StringStream *self = (StringStream *)stream;
    return (SourceLoc) {
        .file = NULL,
        .row = self->row,
        .col = self->col,
    };
}

static void StringStream_Close(Stream *stream)
{
    free(stream);
}

static Status StringStream_New(
    Blimp *blimp, const char *str, StringStream **self)
{
    TRY(Malloc(blimp, sizeof(StringStream), self));
    **self = (StringStream) {
        .base = {
            .Next     = StringStream_Next,
            .Location = StringStream_Location,
            .Close    = StringStream_Close,
        },
        .str = str,
        .row = 0,
        .col = 0,
    };
    return BLIMP_OK;
}

static Status Stream_Peek(Stream *stream, int *c)
{
    if (!stream->peek_valid) {
        stream->peek_loc = stream->Location(stream);
        TRY(stream->Next(stream, &stream->peek));
        stream->peek_valid = true;
    }
    *c = stream->peek;
    return BLIMP_OK;
}

static Status Stream_Next(Stream *stream, int *c)
{
    Status ret = Stream_Peek(stream, c);
    stream->peek_valid = false;
    return ret;
}

static Status Stream_Consume(Stream *stream, int expected)
{
    int c;
    TRY(Stream_Next(stream, &c));
    assert(c == expected);
    return BLIMP_OK;
}

static SourceLoc Stream_Location(Stream *stream)
{
    if (stream->peek_valid) {
        return stream->peek_loc;
    } else {
        return stream->Location(stream);
    }
}

static void Stream_Delete(Stream *stream)
{
    stream->Close(stream);
}

////////////////////////////////////////////////////////////////////////////////
// Lexer
//

typedef enum {
    TOK_BIND,
    TOK_LBRACE,
    TOK_RBRACE,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_PIPE,
    TOK_SEMI,
    TOK_SYMBOL,
    TOK_EOF,
    TOK_INVALID,
} TokenType;

typedef struct {
    TokenType type;
    const Symbol *symbol;
    SourceRange range;
} Token;

typedef struct {
    Blimp *blimp;
    Stream *input;
    Token peek;
} Lexer;

static bool IsOperatorChar(int c)
{
    switch (c) {
        case '~':
        case '`':
        case '!':
        case '@':
        case '$':
        case '%':
        case '^':
        case '&':
        case '*':
        case '-':
        case '=':
        case '+':
        case '[':
        case ']':
        case '\\':
        case ':':
        case '\'':
        case '"':
        case ',':
        case '<':
        case '.':
        case '>':
        case '/':
        case '?':
            return true;
        default:
            return false;
    }
}

static bool IsIdentifierChar(int c)
{
    return c == '_'
        || ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || ('0' <= c && c <= '9');
}

static const char *StringOfTokenType(TokenType t)
{
    switch (t) {
        case TOK_BIND:   return "`bind'";
        case TOK_LBRACE: return "`{'";
        case TOK_RBRACE: return "`}'";
        case TOK_LPAREN: return "`('";
        case TOK_RPAREN: return "`)'";
        case TOK_PIPE:   return "`|'";
        case TOK_SEMI:   return "`;'";
        case TOK_SYMBOL: return "symbol";
        case TOK_EOF:    return "end of input";
        default:         return "invalid token";
    }
}

static void Lexer_Init(Lexer *lex, Blimp *blimp, Stream *input)
{
    lex->blimp = blimp;
    lex->input = input;
    lex->peek.type = TOK_INVALID;
}

static Status Lexer_Peek(Lexer *lex, Token *tok)
{
    if (lex->peek.type != TOK_INVALID) {
        // If we already have a peeked token buffered, just return that.
        *tok = lex->peek;
        return BLIMP_OK;
    }

    // No peeked token available, read a new token from the stream.
    int c;
    char *sym_name;
    size_t sym_length = 0, sym_capacity = 8;

    tok->symbol = NULL;

    // Skip whitespace.
    do {
        tok->range.start = Stream_Location(lex->input);
        tok->range.end   = tok->range.start;
        TRY(Stream_Next(lex->input, &c));
    } while (isspace(c));

    switch (c) {
        case '{':
            tok->type = TOK_LBRACE;
            goto success;
        case '}':
            tok->type = TOK_RBRACE;
            goto success;
        case '(':
            tok->type = TOK_LPAREN;
            goto success;
        case ')':
            tok->type = TOK_RPAREN;
            goto success;
        case '|':
            tok->type = TOK_PIPE;
            goto success;
        case ';':
            tok->type = TOK_SEMI;
            goto success;
        case '#':
            // # starts a line comment, which should be treated as whitespace
            // and skipped by the lexer. To accomplish this, we will consume
            // characters until we see the end of the line:
            do {
                TRY(Stream_Next(lex->input, &c));
            } while (c != '\n' && c != EOF);

            if (c == EOF) {
                // If the comment terminated with EOF rather than a newline,
                // than we actually do produce a token here:
                tok->type = TOK_EOF;
                goto success;
            }

            // Otherwise, recursively peek the next non-whitespace token.
            return Lexer_Peek(lex, tok);
        case EOF:
            tok->type = TOK_EOF;
            goto success;
        default:
            // Any other character must be the start of a symbol or keyword. We
            // will build up a string containing the text of the token, and then
            // check if that string is a keyword (the only one is `bind`) or a
            // symbol.
            TRY(Malloc(lex->blimp, sym_capacity, &sym_name));
            sym_name[sym_length++] = c;
    }

    // We're lexing a symbol. A bl:mp symbol consists of any number of operator
    // characters followed by any number of identifier characters.
    if (IsOperatorChar(c)) {
        // If the first character was an operator, consume as many more operator
        // characters as we can.
        while (true) {
            TRY(Stream_Peek(lex->input, &c));
                // We have to peek before consuming the character, because,
                // unlike in the switch statement above, we are not definitely
                // going to consume a character here. We only want to consume it
                // if it is another operator character.
            if (!IsOperatorChar(c)) break;

            tok->range.end = Stream_Location(lex->input);
            Stream_Consume(lex->input, c);

            if (sym_length >= sym_capacity) {
                TRY(Realloc(lex->blimp, 2*sym_capacity, &sym_name));
                sym_capacity *= 2;
            }
            sym_name[sym_length++] = c;
        }

    } else if (IsIdentifierChar(c)) {
        // If the first character was an identifier character, than this symbol
        // may not contain any operator characters. In this case, we just fall
        // through to consuming the second part of the symbol: the remaining
        // identifier characters.
    } else {
        // Invalid character.
        return ErrorAt(lex->blimp, Stream_Location(lex->input),
            BLIMP_INVALID_CHARACTER, "invalid character in symbol: %c", c);
    }

    // Now that we've consumed the first part of the symbol (the operator
    // characters) consume as many identifier characters as we can.
    while (true) {
        TRY(Stream_Peek(lex->input, &c));
        if (!IsIdentifierChar(c)) break;

        tok->range.end = Stream_Location(lex->input);
        Stream_Consume(lex->input, c);

        if (sym_length >= sym_capacity) {
            TRY(Realloc(lex->blimp, 2*sym_capacity, &sym_name));
            sym_capacity *= 2;
        }
        sym_name[sym_length++] = c;
    }

    // Append a null terminator to the string.
    if (sym_length >= sym_capacity) {
        TRY(Realloc(lex->blimp, sym_capacity+1, &sym_name));
        sym_capacity += 1;
    }
    sym_name[sym_length++] = '\0';

    // Check if we actually lexed a symbol, or if it was a keyword.
    if (strcmp("bind", sym_name) == 0) {
        tok->type = TOK_BIND;
    } else {
        tok->type = TOK_SYMBOL;
        TRY(Blimp_GetSymbol(lex->blimp, sym_name, &tok->symbol));
    }
    Free(lex->blimp, &sym_name);
        // We don't need the string anymore. We either ignore it (for keywords) or
        // converted it to a Symbol (for symbols).

success:
    lex->peek = *tok;
    return BLIMP_OK;
}

static Status Lexer_Next(Lexer *lex, Token *tok)
{
    Status ret = Lexer_Peek(lex, tok);
    lex->peek.type = TOK_INVALID;
    return ret;
}

static Status Lexer_ConsumeWithLoc(
    Lexer *lex, TokenType expected_token, SourceLoc *loc)
{
    Token tok;
    TRY(Lexer_Next(lex, &tok));
    if (tok.type != expected_token) {
        return ErrorFrom(lex->blimp, tok.range, BLIMP_UNEXPECTED_TOKEN,
            "unexpected %s: expecting %s",
            StringOfTokenType(tok.type),
            StringOfTokenType(expected_token));
    }

    if (loc) {
        *loc = tok.range.end;
    }

    return BLIMP_OK;
}

static Status Lexer_Consume(Lexer *lex, TokenType expected_token)
{
    return Lexer_ConsumeWithLoc(lex, expected_token, NULL);
}

////////////////////////////////////////////////////////////////////////////////
// Parser
//

static Status ParseExpr(Lexer *lex, Expr **expr);
static Status ParseStmt(Lexer *lex, Expr **stmt);
static Status ParseTerm(Lexer *lex, Expr **term);
static Status TryTerm(Lexer *lex, Expr **term);

static Status ParseExpr(Lexer *lex, Expr **expr)
{
    // The <expr> non-terminal consists of two productions:
    //
    //  <expr> ::= <stmt> ';' <expr>
    //          |  <stmt>
    //
    // Either way, we parse a statement first.
    Expr *fst;
    TRY(ParseStmt(lex, &fst));

    // Now we look for either a semicolon (first production). If we don't get
    // it, we just stop parsing (second production).
    Token tok;
    TRY(Lexer_Peek(lex, &tok));
    switch (tok.type) {
        case TOK_SEMI: {
            TRY(Lexer_Consume(lex, tok.type));

            // This is a sequence expression. Parse the right-hand side.
            Expr *snd;
            TRY(ParseExpr(lex, &snd));

            // Construct a new expression representing the sequence `fst ; snd`.
            TRY(Malloc(lex->blimp, sizeof(Expr), expr));
            (*expr)->tag = EXPR_SEQ;
            (*expr)->range = (SourceRange) { fst->range.start, snd->range.end };
            (*expr)->seq.fst = fst;
            (*expr)->seq.snd = snd;

            return BLIMP_OK;
        }

        default:
            // This is just an expression by itself, return it.
            *expr = fst;
            return BLIMP_OK;
    }
}

static Status ParseStmt(Lexer *lex, Expr **expr)
{
    // The <stmt> non-terminal consists of two productions:
    //
    //  <stmt> ::= <stmt> <term>
    //          |  <term>
    //
    // The second production is simple: we just delegate to ParseTerm. The first
    // production is the tricky left-recursive case. We handle this production
    // by parsing terms one at a time, left-to-right, and folding them into a
    // statement as we go. In either case, though, we must first parse at least
    // one term.
    TRY(ParseTerm(lex, expr));

    // Parse as many more terms as we can, folding them together as we go into a
    // left associative tree of message sends.
    while (true) {
        Expr *message, *receiver;

        TRY(TryTerm(lex, &message));
        if (!message) {
            // If we can't parse any more terms, return what we have.
            return BLIMP_OK;
        }

        receiver = *expr;
            // The receiver of the message is just whatever expression we have
            // parsed so far. Save it.
        TRY(Malloc(lex->blimp, sizeof(Expr), expr));
            // Replace the top-level parse result with a new expression, which
            // will represent a send of `message` to `receiver`.
        (*expr)->tag = EXPR_SEND;
        (*expr)->range = (SourceRange)
            { receiver->range.start, message->range.end };
        (*expr)->send.receiver = receiver;
        (*expr)->send.message  = message;
    }
}

static Status ParseTerm(Lexer *lex, Expr **term)
{
    TRY(Malloc(lex->blimp, sizeof(Expr), term));

    // The <term> non-terminal consists of four productions:
    //
    //  <term> ::= <symbol>
    //          |  '{' <expr> '|' <expr> '}'
    //          |  '(' <expr> ')'
    //          | 'bind' <term> <term> <term>
    //
    // Each of these productions is distinguished by its first token, so we can
    // go ahead and consume a token, and inspect it to see which production we
    // are trying to parse.
    Token tok;
    TRY(Lexer_Next(lex, &tok));
    (*term)->range = tok.range;
        // We'll update the end of this range later if we have to, but the start
        // should be accurate.

    switch (tok.type) {
        case TOK_LPAREN:
            TRY(ParseExpr(lex, term));
            (*term)->range.start = tok.range.start;
                // ParseExpr overwrites the source range, so we have to reset it
                // here to include the open parenthesis.
            TRY(Lexer_ConsumeWithLoc(lex, TOK_RPAREN, &(*term)->range.end));
            return BLIMP_OK;

        case TOK_LBRACE:
            (*term)->tag = EXPR_BLOCK;
            TRY(Malloc(lex->blimp, sizeof(Expr), &(*term)->block.tag));
            TRY(Malloc(lex->blimp, sizeof(Expr), &(*term)->block.code));

            TRY(ParseExpr(lex, &(*term)->block.tag));
            TRY(Lexer_Consume(lex, TOK_PIPE));
            TRY(ParseExpr(lex, &(*term)->block.code));
            TRY(Lexer_ConsumeWithLoc(lex, TOK_RBRACE, &(*term)->range.end));

            return BLIMP_OK;

        case TOK_SYMBOL:
            (*term)->tag = EXPR_SYMBOL;
            (*term)->symbol = tok.symbol;
            return BLIMP_OK;

        case TOK_BIND:
            TRY(Malloc(lex->blimp, sizeof(Expr), term));
            (*term)->tag = EXPR_BIND;

            TRY(ParseTerm(lex, &(*term)->bind.receiver));
            TRY(ParseTerm(lex, &(*term)->bind.message));
            TRY(ParseTerm(lex, &(*term)->bind.code));

            (*term)->range.end = (*term)->bind.code->range.end;

            return BLIMP_OK;

        default:
            return ErrorFrom(lex->blimp, tok.range, BLIMP_UNEXPECTED_TOKEN,
                "unexpected %s: expecting term", StringOfTokenType(tok.type));
    }
}

static Status TryTerm(Lexer *lex, Expr **term)
{
    Token tok;

    // Check if the next token is one of the four that can start a <term>.
    TRY(Lexer_Peek(lex, &tok));
    switch (tok.type) {
        case TOK_LPAREN:
        case TOK_LBRACE:
        case TOK_SYMBOL:
        case TOK_BIND:
            return ParseTerm(lex, term);
        default:
            *term = NULL;
            return BLIMP_OK;
    }
}

////////////////////////////////////////////////////////////////////////////////
// API
//

Status Blimp_FileStream(Blimp *blimp, const char *path, Stream **stream)
{
    return FileStream_New(blimp, path, (FileStream **)stream);
}

Status Blimp_StringStream(Blimp *blimp, const char *str, Stream **stream)
{
    return StringStream_New(blimp, str, (StringStream **)stream);
}

Status Blimp_Parse(Blimp *blimp, Stream *input, Expr **output)
{
    Lexer lex;
    Lexer_Init(&lex, blimp, input);

    Status ret = ParseExpr(&lex, output);
    if (ret == BLIMP_OK) {
        // Check that we consumed all the input.
        Token tok;
        ret = Lexer_Next(&lex, &tok);
        if (ret == BLIMP_OK && tok.type != TOK_EOF) {
            ret = ErrorFrom(blimp, tok.range, BLIMP_UNEXPECTED_TOKEN,
                "unexpected %s: expecting %s or %s",
                StringOfTokenType(tok.type),
                StringOfTokenType(TOK_SEMI),
                StringOfTokenType(TOK_EOF));
        }
    }

    Stream_Delete(input);
    return ret;
}

Status Blimp_ParseFile(Blimp *blimp, const char *path, Expr **output)
{
    Stream *stream;
    TRY(Blimp_FileStream(blimp, path, &stream));
    return Blimp_Parse(blimp, stream, output);
}

Status Blimp_ParseString(Blimp *blimp, const char *str, Expr **output)
{
    Stream *stream;
    TRY(Blimp_StringStream(blimp, str, &stream));
    return Blimp_Parse(blimp, stream, output);
}
