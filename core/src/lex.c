////////////////////////////////////////////////////////////////////////////////
// The bl:mp Lexer
//
// The implementation of the bl:mp frontend is divided into three
// sub-components:
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
// The lexer implements its own stream-based API, consuming a Stream of
// characters and producing a stream of Tokens. It is not a separate pass over
// the input, but rather a stream that traverses the input stream and feeds
// tokens to the parser simultaneously.
//
// Just as the bl:mp parser can be extended to add new grammar rules, the lexer
// can be extended to add new tokens. To facilitate this, the lexer maintains a
// data structure describing the current set of tokens and how to match them
// against input. This data structure is a state machine, where each state has
// transitions on input characters to other states as well as an optional label
// naming the type of token to emit, if it is an accepting state.
//
// Using this kind of state machine-based approach, we could allow tokens to be
// described by arbitrary regular expressions (since regular expressions can be
// matched by a DFA). However, with the exception of a few special, built-in
// tokens whose matching paths through the state machine contain cycles, all
// bl:mp tokens are described by literal strings, so for simplicity our matching
// state machine is a modified trie, where the few special cases (for example,
// TOK_SYMBOL = r"IdentiferChar+|OperatorChar+") are represented by trie nodes
// which are children of themselves (for example, the `TOK_SYMBOL` node is its
// own child for characters matching 'IdentifierChar').
//
// Another special case regarding built-in tokens is the handling of symbols.
// Any sequence of consecutive identifier characters (see IsIdentifierChar()) or
// consecutive operator characters (see IsOperatorChar()) lexes as a symbol
// token _unless_ a prefix of the sequence matches an overriding token, in which
// case the prefix is lexed as that token. These kinds of "fallback" tokens
// (identifiers and operators) are not represented explicitly in the matching
// trie, because if they were they might conflict with tokens which are supposed
// to override them. Instead, there is a special case in the matching algorithm
// which emits a TOK_SYMBOL token if the input is an identifier or operator that
// does not match any other tokens.
//
// Once we have this trie data structure, the matching algorithm is fairly
// straightforward. We simply feed the input to the state machine, one character
// at a time, for as long as the next character corresponds to a valid state
// transition. When we can go no further, we emit a token corresponding to the
// last accepting state we passed through, or TOK_SYMBOL if we never passed
// through an accepting state and the input is an identifier or operator.
//
// Adding a new token corresponding to a string is also simple. We traverse the
// state machine as if we were tokenizing the string, but if and when we reach a
// state where there is no transition for the next input character, we create a
// transition to a new state and keep going. At the end, if we are in a state
// that is not already an accepting state, we label the state with a fresh token
// type.
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
// Tokens
//

bool IsOperatorChar(int c)
{
    switch (c) {
        case '~':
        case '!':
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

bool IsIdentifierChar(int c)
{
    return c == '_'
        || ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || ('0' <= c && c <= '9');
}

// Static, isolated state machines matching identifier symbols (IdentifierChar+)
// and operator symbols (OperatorChar+). These are not part of the overall
// token-matching state machine because they may conflict with more specific
// tokens. These are only to be used if a prefix of the input does not match a
// specific token in the overall machine.
//
// Call InitStaticTokens() to ensure these are initialized before trying to use
// them.
TrieNode tok_identifier;
TrieNode tok_operator;

void InitStaticTokens(void)
{
    static bool initialized = false;

    if (initialized) {
        return;
    }

    // Initialize `tok_identifier` to represent the regular expression
    // "IdentifierChar*". The lexer will check that the next input character is
    // an identifier character before trying to match with `tok_identifier`, so
    // this will effectively only be used to match "IdentifierChar+".
    tok_identifier.terminal = TOK_SYMBOL;
    for (int c = 0; c < NUM_CHARS; ++c) {
        if (IsIdentifierChar(c)) {
            tok_identifier.children[c] = &tok_identifier;
                // There is a self-transition on every identifier character.
        } else {
            tok_identifier.children[c] = NULL;
        }
    }

    // Initialize `tok_operator` to represent the regular expression
    // "OperatorChar*". The lexer will check that the next input character is an
    // identifier character before trying to match with `tok_operator`, so this
    // will effectively only be used to match "OperatorChar+".
    tok_operator.terminal = TOK_SYMBOL;
    for (int c = 0; c < NUM_CHARS; ++c) {
        if (IsOperatorChar(c)) {
            tok_operator.children[c] = &tok_operator;
                // There is a self-transition on every operator character.
        } else {
            tok_operator.children[c] = NULL;
        }
    }

    initialized = true;
}

static Status NewTrieNode(Blimp *blimp, TrieNode **node)
{
    TRY(Malloc(blimp, sizeof(TrieNode), node));
    (*node)->terminal = TOK_INVALID;
        // By default, this is not an accepting state. The caller can set an
        // accepting terminal type later.
    (*node)->handler = NULL;
        // By default, there is no handler; the matched text will be used as-is
        // for the symbol representing the token. The caller can set a handler
        // if they want one.
    memset((*node)->children, 0, sizeof((*node)->children));
        // Set all of the children to `NULL`, meaning there are no transitions
        // out of this state unless explicitly added later.
    return BLIMP_OK;
}

static Status SymbolHandler(
    Blimp *blimp, const char *escaped, size_t length, char **unescaped)
{
    assert(length >= 2);
    assert(escaped[0] == '`');
    assert(escaped[length-1] == '`');

    // Make a copy of the escaped string, where we will process escape
    // characters in place. In making the copy, we leave out the leading and
    // trailing `s, indicating that this was an escaped string.
    TRY(Strndup(blimp, &escaped[1], length-2, unescaped));

    // Walk the string with a read pointer and a write pointer (which is always
    // equal to the read pointer or lagging behind it). Remove each unescaped
    // backslash, and advance for all non-backslash characters.
    char *write = *unescaped;
    for (char *read = *unescaped; *read != '\0'; ++read) {
        if (*read == '\\') {
            // If we see an unescaped backslash, advance the read pointer but
            // not the write pointer, effectively deleting the backslash.
            ++read;
            assert(*read != '\0');
        }

        // Now `*read` is either an escaped character or an unescaped
        // non-special character, so copy it to `write`.
        *write++ = *read;
    }

    *write = '\0';
        // `write` may have ended up somewhat short of the end of the string, so
        // create a new end.

    return BLIMP_OK;
}

Status TokenTrie_Init(Blimp *blimp, TokenTrie *trie)
{
    InitStaticTokens();

    trie->blimp = blimp;
    trie->num_terminals = NUM_BUILT_IN_TOKENS;

    memset(trie->nodes, 0, sizeof(trie->nodes));
        // Set all the nodes to `NULL`, so that initially there are no
        // transitions out of the starting state except the ones we explicitly
        // add below.

    // Add a state matching "^" for the built-in token TOK_MSG_THIS.
    TrieNode **tok_msg_this = &trie->nodes['^'];
    TRY(NewTrieNode(trie->blimp, tok_msg_this));
    (*tok_msg_this)->terminal = TOK_MSG_THIS;

    // Add a state to handle the built-in terminal TOK_MSG_NAME, which matches
    // the regular expression "\^IdentifierChar+".
    TrieNode *tok_msg_name;
        // This state will match "IdentifierChar*". It is reached from "^" after
        // seeing an identifier character, so overall it represents the regular
        // expression "\^IdentifierChar+", and thus it is accepting.
    TRY(NewTrieNode(trie->blimp, &tok_msg_name));
    tok_msg_name->terminal = TOK_MSG_NAME;
    for (int c = 0; c < NUM_CHARS; ++c) {
        if (IsIdentifierChar(c)) {
            (*tok_msg_this)->children[c] = tok_msg_name;
                // We can reach this state from the "^" state.
            tok_msg_name->children[c] = tok_msg_name;
                // We have a self-transition to greedily consume more identifier
                // characters.
        }
    }

    // Add tokens matching escaped (``-enclosed) symbol literals. The regular
    // expression for this token is not so simple: "`([^`\\]|\\.)*`". That is:
    // a backtick followed by a sequence of either "not a backtick or backslash"
    // or "a backslash followed by anything", terminated by another backtick.
    TrieNode *tok_symbol_end;
        // The accepting state for this token, which we will reach after the
        // closing backtick.
    TRY(NewTrieNode(trie->blimp, &tok_symbol_end));
    tok_symbol_end->terminal = TOK_SYMBOL;
    tok_symbol_end->handler = SymbolHandler;
    TrieNode *tok_symbol;
        // The main state we are in while lexing a symbol literal. Consumes any
        // character that isn't a backslash or backtick. Transitions to
        // `tok_escape` to handle backslash escape characters and to
        // `tok_symbol_end` to handle backticks.
    TrieNode *tok_escape;
        // State reached after seeing a backslash in the middle of a symbol
        // literal. This state will match one additional character no matter
        // what it is, and then return to the `tok_symbol` state.
    TRY(NewTrieNode(trie->blimp, &tok_symbol));
    TRY(NewTrieNode(trie->blimp, &tok_escape));
    for (int c = 0; c < NUM_CHARS; ++c) {
        tok_escape->children[c] = tok_symbol;
            // Any character encountered in the `tok_escape` state causes a
            // transition back to `tok_symbol`.
        switch (c) {
            case '\\':
                tok_symbol->children[c] = tok_escape;
                    // When we see a backslash, we handle an escape character.
                break;
            case '`':
                tok_symbol->children[c] = tok_symbol_end;
                    // When we see an unescaped backtick, we have reached the
                    // end of the symbol.
                break;
            default:
                tok_symbol->children[c] = tok_symbol;
                    // Any other character we just consume and keep going.
                break;
        }
    }
    trie->nodes['`'] = tok_symbol;
        // We reach the `tok_symbol` state from the initial state by lexing an
        // opening backtick.

    // Add a state to handle whitespace ("\s+").
    TrieNode *tok_whitespace;
    TRY(NewTrieNode(trie->blimp, &tok_whitespace));
    tok_whitespace->terminal = TOK_WHITESPACE;
    for (int c = 0; c < NUM_CHARS; ++c) {
        if (isspace(c)) {
            trie->nodes[c] = tok_whitespace;
                // We reach the `tok_whitespace` state from the initial state if
                // we see any whitespace character.
            tok_whitespace->children[c] = tok_whitespace;
                // If we are already lexing a whitespace token and we see a
                // whitespace character, we consume it and keep going.
        }
    }

    // Add a state to handle line comments ("#.*") which are treated as
    // whitespace.
    TrieNode **tok_comment = &trie->nodes['#'];
        // We reach the comment state from the initial state by seeing a "#".
    TRY(NewTrieNode(trie->blimp, tok_comment));
    (*tok_comment)->terminal = TOK_WHITESPACE;
        // A comment is treated as whitespace.
    for (int c = 0; c < NUM_CHARS; ++c) {
        if (c == '\n' || c == EOF_CHAR) {
            // A newline or end-of-file terminates the comment.
            continue;
        }
        (*tok_comment)->children[c] = *tok_comment;
            // Any other character transitions right back to the `tok_comment`
            // state.
    }

    // Add a state to handle the special TOK_EOF terminal, which matches
    // "EOF_CHAR".
    TrieNode **tok_eof = &trie->nodes[EOF_CHAR];
    TRY(NewTrieNode(trie->blimp, tok_eof));
    (*tok_eof)->terminal = TOK_EOF;

    // Add a state to handle TOK_BANG.
    TrieNode **tok_bang = &trie->nodes['!'];
    TRY(NewTrieNode(trie->blimp, tok_bang));
    (*tok_bang)->terminal = TOK_BANG;

    return BLIMP_OK;
}

void TokenTrie_Destroy(TokenTrie *trie)
{
    (void)trie;
}

Status TokenTrie_GetToken(const TokenTrie *trie, const char *string, Token *tok)
{
    // Follow `string` all the way through `trie` and see if we end up in an
    // accepting state (or any state).
    TrieNode *const (*curr)[NUM_CHARS] = &trie->nodes;
        // Pointer to the `children` array of the current state.
    TrieNode *node = NULL;
        // The state we are currently in.
    for (const char *c = string; *c; ++c) {
        node = (*curr)[(int)*c];
        if (node == NULL) {
            break;
        }

        curr = &node->children;
    }

    tok->type = node == NULL || node->terminal == TOK_INVALID
        ? TOK_SYMBOL
            // If there is no state corresponding to `string`, or if there is
            // but it is not an accepting state, then treat `string` as if it
            // had backticks around it and return TOK_SYMBOL.
        : node->terminal;
            // Otherwise, get the token type from the accepting state.

    memset(&tok->range, 0, sizeof(tok->range));
    TRY(HandleToken(
        trie->blimp,
        node == NULL ? NULL : node->handler,
        string,
        strlen(string),
        &tok->symbol
    ));

    return BLIMP_OK;
}

Status TokenTrie_InsertToken(TokenTrie *trie, const char *string, Token *tok)
{
    assert(*string != '\0');

    // Follow `string` all the way through `trie`. If we ever reach a `NULL`,
    // transition, we will create a new state which is (initially) dedicated to
    // matching `string`, so that we can continue matching.
    TrieNode *(*curr)[NUM_CHARS] = &trie->nodes;
        // Pointer to the `children` array of the current state.
    TrieNode **node = NULL;
        // The state we are currently in.
    for (const char *c = string; *c; ++c) {
        node = &(*curr)[(unsigned char)*c];
        if (*node == NULL) {
            // If there is no transition out of the current state on `c`, add
            // one.
            TRY(NewTrieNode(trie->blimp, node));
        }

        curr = &(*node)->children;
    }
    if ((*node)->terminal == TOK_INVALID) {
        // If we ended up in a non-accepting state, create a new terminal which
        // corresponds only to `string`, and make the state accept that
        // terminal.
        (*node)->terminal = trie->num_terminals++;
    }

    tok->type = (*node)->terminal;
    memset(&tok->range, 0, sizeof(tok->range));
    TRY(HandleToken(
        trie->blimp, (*node)->handler, string, strlen(string), &tok->symbol));

    return BLIMP_OK;
}

const char *StringOfTokenType(TokenType t)
{
    switch (t) {
        case TOK_MSG_NAME:  return "^symbol";
        case TOK_MSG_THIS:  return "^";
        case TOK_SYMBOL:    return "symbol";
        case TOK_EOF:       return "end of input";
        default:            return "invalid token";
    }
}

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

Status Stream_Next(Stream *stream, int *c)
{
    TRY(stream->Next(stream, c));
    if (*c == EOF) {
        *c = EOF_CHAR;
            // Translate EOF to EOF_CHAR.
    }

    return BLIMP_OK;
}

SourceLoc Stream_Location(Stream *stream)
{
    return stream->Location(stream);
}

void Stream_Delete(Stream *stream)
{
    stream->Close(stream);
}

////////////////////////////////////////////////////////////////////////////////
// Lexer
//

void Lexer_Init(Lexer *lex, Blimp *blimp, Stream *input)
{
    lex->blimp = blimp;
    lex->input = input;

    Vector_Init(blimp, &lex->look_ahead_chars, sizeof(char), NULL);
    Vector_Init(blimp, &lex->look_ahead_locs, sizeof(SourceLoc), NULL);
    lex->look_ahead_end = 0;
    lex->peeked_len = 0;
    lex->eof = false;
    lex->tokens = &blimp->tokens;
}

void Lexer_Destroy(Lexer *lex)
{
    Vector_Destroy(&lex->look_ahead_chars);
    Vector_Destroy(&lex->look_ahead_locs);
}

Status HandleToken(
    Blimp *blimp,
    TokenHandler handler,
    const char *string,
    size_t length,
    const Symbol **sym)
{
    if (handler == NULL) {
        // If there is no handler, just use the given string as-is.
        return Blimp_GetSymbolWithLength(blimp, string, length, sym);
    }

    // If there is a handler, call it to get a new, processed, name.
    char *name;
    TRY(handler(blimp, string, length, &name));

    Status ret = Blimp_GetSymbol(blimp, name, sym);

    free(name);
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
