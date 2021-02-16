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
// TOK_PRECEDENCE = r"@IdentiferChar+") are represented by trie nodes which are
// children of themselves (for example, the `TOK_PRECEDENCE` node is its own
// child for characters matching 'IdentifierChar').
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

static bool IsOperatorChar(int c)
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

static bool IsIdentifierChar(int c)
{
    return c == '_'
        || ('a' <= c && c <= 'z')
        || ('A' <= c && c <= 'Z')
        || ('0' <= c && c <= '9');
}

// A TrieNode is a state in the token-matching state machine.
typedef struct TrieNode {
    Terminal terminal;
        // The token type matched by this state, or TOK_INVALID if this is not
        // an accepting state.
    struct TrieNode *children[NUM_CHARS];
        // States to transition to indexed by the next character we see. If a
        // character `c` is not expessed, then `children[c]` is `NULL`.
} TrieNode;

// Static, isolated state machines matching identifier symbols (IdentifierChar+)
// and operator symbols (OperatorChar+). These are not part of the overall
// token-matching state machine because they may conflict with more specific
// tokens. These are only to be used if a prefix of the input does not match a
// specific token in the overall machine.
//
// Call InitStaticTokens() to ensure these are initialized before trying to use
// them.
static TrieNode tok_identifier;
static TrieNode tok_operator;

static void InitStaticTokens(void)
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
    memset((*node)->children, 0, sizeof((*node)->children));
        // Set all of the children to `NULL`, meaning there are no transitions
        // out of this state unless explicitly added later.
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

    // Add states to handle the built-in terminal TOK_PRECEDENCE, which matches
    // the regular expression "@IdentifierChar+".
    TrieNode *tok_precedence_id_star;
        // This state will match "IdentifierChar*", and will accept the input.
    TRY(NewTrieNode(trie->blimp, &tok_precedence_id_star));
    tok_precedence_id_star->terminal = TOK_PRECEDENCE;
    for (int c = 0; c < NUM_CHARS; ++c) {
        if (IsIdentifierChar(c)) {
            tok_precedence_id_star->children[c] = tok_precedence_id_star;
        }
    }
    TrieNode **tok_precedence = &trie->nodes['@'];
        // This state (reached after seeing an "@") will match "IdentifierChar+"
        // by first matching one "IdentifierChar" and then transitioning to the
        // "IdentifierChar*" accepting state above. This state itself is not
        // accepting, because in this state we have not seen at least one
        // "IdentifierChar".
    TRY(NewTrieNode(trie->blimp, tok_precedence));
    for (int c = 0; c < NUM_CHARS; ++c) {
        if (IsIdentifierChar(c)) {
            (*tok_precedence)->children[c] = tok_precedence_id_star;
        }
    }

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
    TRY(Blimp_GetSymbol(trie->blimp, string, &tok->symbol));

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
    TRY(Blimp_GetSymbol(trie->blimp, string, &tok->symbol));

    return BLIMP_OK;
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

static Status Stream_Next(Stream *stream, int *c)
{
    TRY(stream->Next(stream, c));
    if (*c == EOF) {
        *c = EOF_CHAR;
            // Translate EOF to EOF_CHAR.
    }

    return BLIMP_OK;
}

static SourceLoc Stream_Location(Stream *stream)
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

const char *StringOfTokenType(TokenType t)
{
    switch (t) {
        case TOK_MSG_NAME:  return "^symbol";
        case TOK_MSG_THIS:  return "^";
        case TOK_SYMBOL:    return "symbol";
        case TOK_EOF:       return "end of input";
        case TOK_PRECEDENCE:return "precedence symbol";
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

    Vector_Init(blimp, &lex->look_ahead_chars, sizeof(char), NULL);
    Vector_Init(blimp, &lex->look_ahead_locs, sizeof(SourceLoc), NULL);
    lex->look_ahead_end = 0;
    lex->eof = false;

    lex->peek.type = TOK_INVALID;
    lex->tokens = &blimp->tokens;
}

void Lexer_Destroy(Lexer *lex)
{
    Vector_Destroy(&lex->look_ahead_chars);
    Vector_Destroy(&lex->look_ahead_locs);
}

static Status Lexer_LookAhead(Lexer *lex, int *c)
{
    if (lex->look_ahead_end < Vector_Length(&lex->look_ahead_chars)) {
        // If there is a character in the look-ahead buffer which has not yet
        // been processed as part of the current token, return it and increment
        // the look-ahead pointer.
        *c = *(char *)Vector_Index(
            &lex->look_ahead_chars, lex->look_ahead_end++);
        return BLIMP_OK;
    }
    if (lex->eof) {
        // If there are no extra characters in the look-ahead buffer and the
        // input stream as at end-of-file, return EOF_CHAR.
        *c = EOF_CHAR;
        return BLIMP_OK;
    }

    // Otherwise, get a character from the input stream.
    SourceLoc loc = Stream_Location(lex->input);
    TRY(Stream_Next(lex->input, c));
    if (*c == EOF_CHAR) {
        lex->eof = true;
            // Remember that the stream has reached end-of-file so we don't try
            // to read more from it.
        return BLIMP_OK;
    }

    char truncated = (char)*c;
        // If the character is not EOF_CHAR, then it is a valid `char`.

    // Add the new character to the look-ahead buffer so we remember it in case
    // we have to backtrack.
    TRY(Vector_PushBack(&lex->look_ahead_chars, &truncated));
    TRY(Vector_PushBack(&lex->look_ahead_locs, &loc));
    ++lex->look_ahead_end;

    return BLIMP_OK;
}

static Status Lexer_Consume(
    Lexer *lex, size_t length, const Symbol **sym, SourceRange *range)
{
    if (length > lex->look_ahead_end) {
        length = lex->look_ahead_end;
            // Since we don't add the EOF character to the look-ahead buffer,
            // `length` can exceed the number of buffered characters if the
            // matched token includes EOF. We will ignore the EOF character in
            // the matched symbol, since it is not a real character anyways.
    }


    assert(lex->look_ahead_end <= Vector_Length(&lex->look_ahead_chars));
    assert(Vector_Length(&lex->look_ahead_chars) ==
           Vector_Length(&lex->look_ahead_locs));

    // Get the symbol corresponding to the prefix of `look_ahead_chars` with the
    // length we're consuming.
    TRY(Blimp_GetSymbolWithLength(
        lex->blimp, Vector_Data(&lex->look_ahead_chars), length, sym));

    if (lex->look_ahead_end == 0) {
        // If nothing is buffered, get the source location from the input
        // stream.
        assert(length == 0);
        range->start = Stream_Location(lex->input);
        range->end = range->start;
    } else {
        // Otherwise, the source range comes from the buffered locations.
        range->start = *(SourceLoc *)Vector_Index(&lex->look_ahead_locs, 0);
        if (length == 0) {
            range->end = range->start;
        } else {
            range->end = *(SourceLoc *)Vector_Index(
                &lex->look_ahead_locs, length-1);
        }
    }

    // Drop the characters and corresponding locations that we consumed.
    Vector_Shift(&lex->look_ahead_chars, length);
    Vector_Shift(&lex->look_ahead_locs, length);
    lex->look_ahead_end = 0;

    return BLIMP_OK;
}

static inline void Lexer_Backtrack(Lexer *lex)
{
    lex->look_ahead_end = 0;
}

static inline size_t Lexer_LookAheadLength(const Lexer *lex)
{
    return lex->look_ahead_end;
}

static inline SourceRange Lexer_LookAheadRange(const Lexer *lex)
{
    if (lex->look_ahead_end == 0) {
        // If nothing is buffered, get the source location from the input
        // stream.
        return (SourceRange) {
            .start = Stream_Location(lex->input),
            .end   = Stream_Location(lex->input),
        };
    } else {
        // Otherwise, the source range comes from the buffered locations.
        return (SourceRange) {
            .start = *(SourceLoc *)Vector_Index(&lex->look_ahead_locs, 0),
            .end   = *(SourceLoc *)Vector_Index(
                            &lex->look_ahead_locs, lex->look_ahead_end-1),
        };
    }
}

static inline const char *Lexer_LookAheadChars(const Lexer *lex)
{
    return Vector_Data(&lex->look_ahead_chars);
}

// A matcher encapsulates a path through a TokenTrie. It can be used to traverse
// a trie and extract a single result at the end.
typedef struct {
    TrieNode *curr;
    size_t curr_len;
    TrieNode *match;
    size_t match_len;
} Matcher;

static inline void Matcher_Init(Matcher *m, TrieNode *root)
{
    m->curr = root;
    m->curr_len = 1;
        // The first character of input determines what root node to use, so by
        // the time we have a root, we have already seen at least 1 character.
    m->match = NULL;
    m->match_len = 0;
}

// Advance the Matcher one character further down the trie. Returns `true` if
// the Matcher might accept more characters.
static inline bool Matcher_Next(Matcher *m, int c)
{
    if (m->curr == NULL) {
        return false;
    }

    if (m->curr->terminal != TOK_INVALID) {
        // If we are currently in an accepting state, record the possible match.
        m->match = m->curr;
        m->match_len = m->curr_len;
    }

    ++m->curr_len;
    m->curr = m->curr->children[c];
    return m->curr != NULL;
}

static inline size_t Matcher_MatchLength(const Matcher *m)
{
    return m->match_len;
        // Returns 0 if there is no match.
}

static inline Terminal Matcher_MatchTerminal(const Matcher *m)
{
    return m->match->terminal;
}

Status Lexer_Peek(Lexer *lex, Token *tok)
{
    InitStaticTokens();

    if (lex->peek.type != TOK_INVALID) {
        // If we already have a peeked token buffered, just return that.
        *tok = lex->peek;
        return BLIMP_OK;
    }

    // No peeked token available, read a new token from the stream. We will keep
    // lexing and ignoring new tokens until we get to a non-whitespace token.
    do {
        int c;
        Matcher tok_match;
        Matcher sym_match;

        // Get the first character in the input, which gives us a root trie
        // node.
        TRY(Lexer_LookAhead(lex, &c));
        Matcher_Init(&tok_match, lex->tokens->nodes[c]);

        // If the first character is an identifier or operator character, then
        // we could match a non-empty prefix of the input with a symbol token.
        // We will proceed with both matching strategies (`tok_match` and
        // `sym_match`) in parallel, and use whichever one ends up being longer
        // if they both match.
        if (IsIdentifierChar(c)) {
            Matcher_Init(&sym_match, &tok_identifier);
        } else if (IsOperatorChar(c)) {
            Matcher_Init(&sym_match, &tok_operator);
        } else {
            Matcher_Init(&sym_match, NULL);
        }

        // Continue reading input until neither matcher can advance further.
        do {
            TRY(Lexer_LookAhead(lex, &c));
        } while (Matcher_Next(&tok_match, c) | Matcher_Next(&sym_match, c));

        // Take the longest match.
        Matcher *match = &tok_match;
        if (Matcher_MatchLength(&sym_match) > Matcher_MatchLength(&tok_match)) {
            match = &sym_match;
        }

        if (Matcher_MatchLength(match) == 0) {
            Lexer_Backtrack(lex);
            return ErrorFrom(lex->blimp, Lexer_LookAheadRange(lex),
                BLIMP_INVALID_CHARACTER,
                "invalid characters '%.*s'",
                (int)Lexer_LookAheadLength(lex),
                Lexer_LookAheadChars(lex)
            );
        }

        // Consume the characters we matched.
        tok->type = Matcher_MatchTerminal(match);
        TRY(Lexer_Consume(
            lex, Matcher_MatchLength(match), &tok->symbol, &tok->range));
    } while (tok->type == TOK_WHITESPACE);

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
