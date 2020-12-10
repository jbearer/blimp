////////////////////////////////////////////////////////////////////////////////
// The bl:mp Lexer
//
// The implementation of the bl:mp frontend is divided into three sub-components:
//  * I/O streams
//  * Lexing
//  * Parsing
//
// This file implements the first two. The parser is complicated and is
// implemented in its own module, lalr.c
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

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "internal/blimp.h"
#include "internal/error.h"
#include "internal/expr.h"
#include "internal/parse.h"

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

void Stream_Delete(Stream *stream)
{
    stream->Close(stream);
}

////////////////////////////////////////////////////////////////////////////////
// Lexer
//

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

const char *StringOfTokenType(TokenType t)
{
    switch (t) {
        case TOK_LBRACE:    return "`{'";
        case TOK_RBRACE:    return "`}'";
        case TOK_LPAREN:    return "`('";
        case TOK_RPAREN:    return "`)'";
        case TOK_SEMI:      return "`;'";
        case TOK_MSG_NAME:  return "^symbol";
        case TOK_MSG_THIS:  return "^";
        case TOK_SYMBOL:    return "symbol";
        case TOK_EOF:       return "end of input";
        default:            return "invalid token";
    }
}

BlimpErrorCode UnexpectedTokenError(TokenType t)
{
    switch (t) {
        case TOK_EOF: return BLIMP_UNEXPECTED_EOF;
        default:      return BLIMP_UNEXPECTED_TOKEN;
    }
}

void Lexer_Init(Lexer *lex, Blimp *blimp, Stream *input)
{
    lex->blimp = blimp;
    lex->input = input;
    lex->peek.type = TOK_INVALID;
}

void Lexer_Destroy(Lexer *lex)
{
    (void)lex;
}

Status Lexer_Peek(Lexer *lex, Token *tok)
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
                // then we actually do produce a token here:
                tok->type = TOK_EOF;
                break;
            }

            // Otherwise, recursively peek the next non-whitespace token.
            return Lexer_Peek(lex, tok);
        case EOF:
            tok->type = TOK_EOF;
            break;
        case '^':
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
                tok->type = TOK_MSG_NAME;

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
                tok->type = TOK_MSG_THIS;
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
                        UnexpectedTokenError(TOK_EOF),
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

Status Lexer_Next(Lexer *lex, Token *tok)
{
    Status ret = Lexer_Peek(lex, tok);
    lex->peek.type = TOK_INVALID;
    return ret;
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
