////////////////////////////////////////////////////////////////////////////////
// The bl:mp Parser
//
// This module contains a recursive-descent implementation of a parser for the
// bl:mp expression grammar:
//
//  <expr> ::= <symbol>
//          |  '{' ('^' <symbol>)? <expr> '}'
//          | '^' <symbol>?
//          | <expr> <expr>
//          |  <expr> ';' <expr>
//
//  <symbol> ::= <operator-char>+
//            |  <identifier-char>+
//            |  '`' .* '`'
//
// This grammar is a fully adequate description of the bl:mp AST, in that any
// valid bl:mp parse tree is produced by this grammar. However, the grammar as
// given is not a sufficient description of the bl:mp concrete syntax, because
// it is ambiguous: the associativity and precedence of the operators such as
// "sequence" (;)  and "send" (<expr> <expr>) are not specified.
//
// To make the grammar unambiguous, this implementation adds two additional
// non-terminals: `term`, which represents "atomic" expressions (e.g. literals
// and parenthesized compound expressions), and `stmt`, which represents a
// single expression in a sequence. We also add an additional production to
// allow parenthesized expressions.
//
//  <expr> ::= <expr> ';' <stmt>
//          |  <stmt>
//
//  <stmt> ::= <stmt> <term>
//          |  <term>
//
//  <term> ::= <symbol>
//          |  '{' ('^' <symbol>)? <expr> '}'
//          | '^' <symbol>?
//          |  '(' <expr> ')'
//
// With this extended grammar, it is possible to see that:
//  * Precedence order is "^", "send", "sequence"
//  * Send is left-associative. This is required by bl:mp semantics.
//  * Sequence is left-associative. This is arbitrary, as ; is semantically
//    associativity. Left-associativity is easier to parse in an iterative way.
//    If we did the naive right-associative recursive descent parsing here, we
//    could overflow the stack when parsing long programs. Note that stack
//    overflow is only really a concern from the <expr> non-terminal, because
//    most programs consist of many fairly small <stmt>s and <term>s strung
//    together with ';' into a very large <expr>.
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
// recursive-descent algorithm. There are, however, two productions that require
// special handling: the left recursion in the <stmt> production for "send"
// (<stmt> <term>) cannot be handled by naive recursive-descent. We handle this
// case with an ad hoc algorithm which parses as many <term>s as it can from
// left to right, folding them into a "send" <expr> as it goes. We handle the
// left-recursion in <expr> similarly, except we do not fold sequences of
// expressions into a recursive tree; instead we represent them using a flat
// list (via the `next` field on the `Expr` datatype) which makes them a bit
// easier to process without blowing up the stack.
//
// The parser's responsibilities also include resolving references to message
// names (^name) to DeBruijn indices; that is, the parser must convert {^a {^b
// ^a } } to {{1}}. To that end, we maintain a DeBruijnMap of the message names
// which are in scope. A new name is pushed into the map whenever we enter a
// block where the name is bound, and popped when we leave the block. This map
// can be searched whenever we encounter a reference to a name to determine (1)
// whether the name is in scope (otherwise it is a parse error) and (2) how many
// nested scopes are between the binding of the name and the use (that is, the
// DeBruijn index of the reference).
//

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "internal/blimp.h"
#include "internal/debruijn.h"
#include "internal/error.h"
#include "internal/expr.h"

////////////////////////////////////////////////////////////////////////////////
// I/O
//

#define PATH_MAX 256

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

static const char *GetPathFromFile(FILE *file)
{
#if _POSIX_C_SOURCE >= 200112L
    char proc_path[PATH_MAX+1];
    if (snprintf(
            proc_path, PATH_MAX+1, "/proc/self/fd/%d", fileno(file))
        > PATH_MAX)
    {
        return NULL;
    }

    char *path = malloc(PATH_MAX+1);
    if (path == NULL) {
        return NULL;
    }

    ssize_t len = readlink(proc_path, path, PATH_MAX+1);
    if (len < 0) {
        free(path);
        return NULL;
    }
    if (len > PATH_MAX) {
        free(path);
        return NULL;
    }
    path[len] = '\0';
    return path;
#else
    (void)file;
    return NULL;
#endif
}

static Status FileStream_New(
    Blimp *blimp, const char *name, FILE *file, FileStream **self)
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
            .file = NULL,
            .row  = 0,
            .col  = 0,
        },
        .f = file,
    };

    // Try to get the real path that actually corresponds to `file`, according
    // to the operating system.
    if (((*self)->loc.file = GetPathFromFile(file)) == NULL) {
        Strdup(blimp, name, (char **)&(*self)->loc.file);
            // If that failed, just use the given `name` (which may be
            // meaningless) as the path.
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
    TOK_LBRACE,
    TOK_RBRACE,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_SEMI,
    TOK_MSG_NAME,
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
    DeBruijnMap scopes;
        // Stack of message names which are currently in scope.
    size_t counter;
        // Monotonic counter used for generating fresh names.
} Lexer;

static bool IsOperatorChar(int c)
{
    switch (c) {
        case '~':
        case '!':
        case '@':
        case '$':
        case '%':
        case '&':
        case '*':
        case '-':
        case '=':
        case '+':
        case '[':
        case ']':
        case '\\':
        case '|':
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
        case TOK_LBRACE:    return "`{'";
        case TOK_RBRACE:    return "`}'";
        case TOK_LPAREN:    return "`('";
        case TOK_RPAREN:    return "`)'";
        case TOK_SEMI:      return "`;'";
        case TOK_MSG_NAME:  return "^symbol";
        case TOK_SYMBOL:    return "symbol";
        case TOK_EOF:       return "end of input";
        default:            return "invalid token";
    }
}

static void Lexer_Init(Lexer *lex, Blimp *blimp, Stream *input)
{
    lex->blimp = blimp;
    lex->input = input;
    lex->peek.type = TOK_INVALID;
    lex->counter = 0;
    DBMap_Init(blimp, &lex->scopes);
}

static void Lexer_Destroy(Lexer *lex)
{
    DBMap_Destroy(&lex->scopes);
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
    char *sym_name = NULL;
    size_t sym_length = 0, sym_capacity = 0;

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
            break;
        case '}':
            tok->type = TOK_RBRACE;
            break;
        case '(':
            tok->type = TOK_LPAREN;
            break;
        case ')':
            tok->type = TOK_RPAREN;
            break;
        case ';':
            tok->type = TOK_SEMI;
            break;
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
                break;
            }

            // Otherwise, recursively peek the next non-whitespace token.
            return Lexer_Peek(lex, tok);
        case EOF:
            tok->type = TOK_EOF;
            break;
        case '^':
            tok->type = TOK_MSG_NAME;

            while (true) {
                TRY(Stream_Peek(lex->input, &c));
                    // We have to peek before consuming the character, because,
                    // since ^ on its own is a valid token, we are not
                    // definitely going to consume a character here. We only
                    // want to consume it if it is part of a message name, which
                    // is restricted to operator characters.
                if (!IsIdentifierChar(c)) break;

                // Now we know we're taking this character. Push the token
                // source range one character farther and then consume the
                // character from the stream.
                tok->range.end = Stream_Location(lex->input);
                Stream_Consume(lex->input, c);

                if (sym_length >= sym_capacity) {
                    sym_capacity = 2*sym_capacity + 1;
                    TRY(Realloc(lex->blimp, sym_capacity, &sym_name));
                }
                sym_name[sym_length++] = c;
            }

            if (sym_length) {
                // Append a null terminator.
                if (sym_length >= sym_capacity) {
                    TRY(Realloc(lex->blimp, sym_length + 1, &sym_name));
                }
                sym_name[sym_length++] = '\0';

                // Create the symbol.
                TRY(Blimp_GetSymbol(lex->blimp, sym_name, &tok->symbol));
                assert(tok->symbol);
                Free(lex->blimp, &sym_name);
            } else {
                tok->symbol = NULL;
            }

            break;
        case '`':
            // ` starts an escaped symbol literal. We will consume all non-`
            // characters until we get to another `.
            while (true) {
                TRY(Stream_Next(lex->input, &c));
                if (c == '`') {
                    break;
                }
                if (c == EOF) {
                    return Blimp_ErrorAt(
                        lex->blimp, Stream_Location(lex->input),
                        BLIMP_UNEXPECTED_TOKEN,
                        "unexpected end of input (expecting ``')"
                    );
                }

                if (sym_length >= sym_capacity) {
                    sym_capacity = 2*sym_capacity + 1;
                    TRY(Realloc(lex->blimp, sym_capacity, &sym_name));
                }
                sym_name[sym_length++] = c;
            }

            // Append a null terminator.
            if (sym_length >= sym_capacity) {
                TRY(Realloc(lex->blimp, sym_length + 1, &sym_name));
            }
            sym_name[sym_length++] = '\0';

            // Create the symbol.
            tok->type = TOK_SYMBOL;
            TRY(Blimp_GetSymbol(lex->blimp, sym_name, &tok->symbol));
            assert(tok->symbol);

            Free(lex->blimp, &sym_name);
            break;

        default: {
            // Any other character must be part of a symbol (or it is invalid).
            // Determine whether this symbols is an operator or an identifier.
            bool(*predicate)(int);
            if (IsOperatorChar(c)) {
                predicate = IsOperatorChar;
            } else if (IsIdentifierChar(c)) {
                predicate = IsIdentifierChar;
            } else {
                // Invalid character.
                return ErrorAt(lex->blimp, Stream_Location(lex->input),
                    BLIMP_INVALID_CHARACTER,
                    "invalid character in symbol: %c", c);
            }

            sym_capacity = 8;
            TRY(Malloc(lex->blimp, sym_capacity, &sym_name));
            sym_name[sym_length++] = c;

            while (true) {
                TRY(Stream_Peek(lex->input, &c));
                    // We have to peek before consuming the character, because,
                    // unlike in the `` case above, we are not definitely going
                    // to consume a character here. We only want to consume it
                    // if it is another operator/identifier character.
                if (!predicate(c)) break;

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

            tok->type = TOK_SYMBOL;
            TRY(Blimp_GetSymbol(lex->blimp, sym_name, &tok->symbol));
            assert(tok->symbol);

            Free(lex->blimp, &sym_name);
                // We don't need the string anymore. We either ignore it (for
                // keywords) or converted it to a Symbol (for symbols).

            break;
        }
    }

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

static inline Status Lexer_PushScope(Lexer *lex, const Symbol *msg_name)
{
    return DBMap_Push(&lex->scopes, (void *)msg_name);
}

static inline void Lexer_PopScope(Lexer *lex)
{
    DBMap_Pop(&lex->scopes);
}

static inline bool Lexer_InBlock(const Lexer *lex)
{
    return !DBMap_Empty(&lex->scopes);
}

static bool Lexer_MessageNameEq(void *sym1, void *sym2)
{
    return SymbolEq((const Symbol **)&sym1, (const Symbol **)&sym2);
}

static inline Status Lexer_ResolveMessageName(
    const Lexer *lex, const Symbol *msg_name, size_t *index)
{
    if (DBMap_Index(
            &lex->scopes, (void *)msg_name, Lexer_MessageNameEq, index)
        != BLIMP_OK)
    {
        return ErrorMsg(lex->blimp, BLIMP_INVALID_MESSAGE_NAME,
            "no message named ^%s is in scope", msg_name->name);
    }

    return BLIMP_OK;
}

static Status Lexer_FreshSymbol(Lexer *lex, const Symbol **symbol)
{
    char buffer[32];
    snprintf(buffer, sizeof(buffer), "^%zu^", lex->counter++);
    return Blimp_GetSymbol(lex->blimp, buffer, symbol);
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
    //  <expr> ::= <expr> ';' <stmt>
    //          |  <stmt>
    //
    // The second production is simple: we just delegate to ParseStmt. The first
    // production is the tricky left-recursive case. We handle this production
    // by parsing statements one at a time, left-to-right, and folding them into
    // a list as we go. In either case, though, we must first parse at least one
    // statement.
    TRY(ParseStmt(lex, expr));

    // Now we look ahead to see if the next token is a semicolon. If it is, we
    // parse another statement and add it to the sequence expression we're
    // building up. We keep doing this until we hit something other than a
    // semicolon.
    Expr *prev = *expr;
    while (true) {
        Token tok;
        TRY(Lexer_Peek(lex, &tok));

        if (tok.type != TOK_SEMI) {
            return BLIMP_OK;
        }

        TRY(Lexer_Consume(lex, tok.type));
            // Consume the peeked token, comitting to the ';' production.

        TRY(ParseStmt(lex, &prev->next));
            // Parse the next statement in the sequence.

        prev = prev->next;
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
        (*expr)->refcount = 1;
        (*expr)->next = NULL;
        (*expr)->range = (SourceRange)
            { receiver->range.start, message->range.end };
        (*expr)->send.receiver = receiver;
        (*expr)->send.message  = message;
    }
}

static Status ParseTerm(Lexer *lex, Expr **term)
{
    TRY(Malloc(lex->blimp, sizeof(Expr), term));
    (*term)->refcount = 1;
    (*term)->next = NULL;

    // The <term> non-terminal consists of four productions:
    //
    //  <term> ::= <symbol>
    //          | '^'<symbol>?
    //          |  '{' ('^'<symbol>)? <expr> '}'
    //          |  '(' <expr> ')'
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
            Free(lex->blimp, term);
            TRY(ParseExpr(lex, term));
            (*term)->range.start = tok.range.start;
                // ParseExpr overwrites the source range, so we have to reset it
                // here to include the open parenthesis.
            TRY(Lexer_ConsumeWithLoc(lex, TOK_RPAREN, &(*term)->range.end));
            return BLIMP_OK;

        case TOK_LBRACE: {
            (*term)->tag = EXPR_BLOCK;

            // Check for an optional message name binder.
            TRY(Lexer_Peek(lex, &tok));
            if (tok.type == TOK_MSG_NAME && tok.symbol != NULL) {
                (*term)->block.msg_name = tok.symbol;
                Lexer_Consume(lex, TOK_MSG_NAME);
            } else {
                // Generate a fresh name for the messages passed to this block.
                TRY(Lexer_FreshSymbol(lex, &(*term)->block.msg_name));
            }

            // Parse the body of the block with the new message name in scope.
            Lexer_PushScope(lex, (*term)->block.msg_name);
            Status status = ParseExpr(lex, &(*term)->block.code);
            Lexer_PopScope(lex);
            if (status != BLIMP_OK) {
                return status;
            }

            TRY(Lexer_ConsumeWithLoc(lex, TOK_RBRACE, &(*term)->range.end));
            return BLIMP_OK;
        }

        case TOK_SYMBOL:
            (*term)->tag = EXPR_SYMBOL;
            (*term)->symbol = tok.symbol;
            return BLIMP_OK;

        case TOK_MSG_NAME:
            (*term)->tag = EXPR_MSG;

            if (tok.symbol == NULL) {
                // A '^' with no name refers to the message name bound in the
                // innermost containing block (DeBruijn index of 0).
                if (!Lexer_InBlock(lex)) {
                    return ErrorFrom(
                        lex->blimp, tok.range, BLIMP_INVALID_MESSAGE_NAME,
                        "^ cannot be used outside of a block");
                }
                (*term)->msg.index = 0;
            } else {
                if (Lexer_ResolveMessageName(
                    lex, tok.symbol, &(*term)->msg.index) != BLIMP_OK)
                {
                    return ReraiseFrom(lex->blimp, tok.range);
                }
            }

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
        case TOK_MSG_NAME:
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
    FILE *file = fopen(path, "r");
    if (file == NULL) {
        return ErrorMsg(
            blimp, BLIMP_IO_ERROR, "cannot open %s: %s", path, strerror(errno));
    }
    return FileStream_New(blimp, path, file, (FileStream **)stream);
}

Status Blimp_OpenFileStream(
    Blimp *blimp, const char *name, FILE *file, Stream **stream)
{
    return FileStream_New(blimp, name, file, (FileStream **)stream);
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

    Lexer_Destroy(&lex);
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
