#ifndef BLIMP_PARSE_H
#define BLIMP_PARSE_H

#include "common.h"
#include "vector.h"

////////////////////////////////////////////////////////////////////////////////
// Lexer
//

PRIVATE Status Stream_Next(Stream *stream, int *c);
PRIVATE SourceLoc Stream_Location(Stream *stream);
PRIVATE void Stream_Delete(Stream *stream);

typedef enum {
    TOK_MSG_NAME,
    TOK_MSG_THIS,
    TOK_SYMBOL,
    TOK_EOF,
    TOK_WHITESPACE,
    TOK_BANG,
    TOK_OBJECT,
    NUM_BUILT_IN_TOKENS,
} BuiltInToken;

typedef size_t TokenType;

#define TOK_INVALID ((TokenType)-1)

typedef struct {
    TokenType type;
    const Symbol *symbol;
    SourceRange range;
} Token;

PRIVATE const char *StringOfTokenType(TokenType t);

#define EOF_CHAR ((int)UCHAR_MAX + 1)
    // The EOF macro may be -1 (or some other negative value). It's convenient
    // to be able to to treat all possible values we might read from a stream as
    // indices into a lookup table, which does not work if `EOF` is negative.
    // Therefore, the lexer uses EOF_CHAR (which is one greater than the maximum
    // unsigned char value) to represent end-of-file, and internally translates
    // EOF to EOF_CHAR when reading from a stream.
#define NUM_CHARS (EOF_CHAR + 1)
    // One for each valid `char`, plus EOF_CHAR.

typedef Status(*TokenHandler)(
    Blimp *blimp, const char *match, size_t length, char **new_match);

// A TrieNode is a state in the token-matching state machine.
typedef struct TrieNode {
    TokenType terminal;
        // The token type matched by this state, or TOK_INVALID if this is not
        // an accepting state.
    TokenHandler handler;
        // Optional function to post-process a matched token string.
    struct TrieNode *children[NUM_CHARS];
        // States to transition to indexed by the next character we see. If a
        // character `c` is not expessed, then `children[c]` is `NULL`.
} TrieNode;

typedef struct {
    Blimp *blimp;
    size_t num_terminals;
    TrieNode *nodes[NUM_CHARS];
} TokenTrie;

PRIVATE extern TrieNode tok_identifier;
PRIVATE extern TrieNode tok_operator;

PRIVATE Status TokenTrie_Init(Blimp *blimp, TokenTrie *trie);
PRIVATE void TokenTrie_Destroy(TokenTrie *trie);
PRIVATE void InitStaticTokens(void);

/**
 * \brief Get a Token with an existing TokenType that matches `string`.
 *
 * This function will not tokenize `string` into multiple tokens or add a new
 * token type that matches `string`. It will find one existing token type that
 * matches all of `string`. If there is no such token type, the result will be
 * TOK_SYMBOL, as if the contents of `string` had been enclosed in backticks.
 */
PRIVATE Status TokenTrie_GetToken(
    const TokenTrie *tokens, const char *string, Token *tok);

/**
 * \brief
 *      Get a Token which matches `string`, creating a new TokenType if
 *      necessary.
 *
 * This function is similar to TokenTrie_GetToken(), except that if there is no
 * existing token type which matches all of `string`, a new token type will be
 * created which matches `string` and nothing else.
 */
PRIVATE Status TokenTrie_InsertToken(
    TokenTrie *tokens, const char *string, Token *tok);

PRIVATE Status HandleToken(
    Blimp *blimp,
    TokenHandler handler,
    const char *string,
    size_t length,
    const Symbol **sym);

PRIVATE bool IsOperatorChar(int c);
PRIVATE bool IsIdentifierChar(int c);

typedef struct {
    Blimp *blimp;
    Stream *input;

    Vector/*<char>*/ look_ahead_chars;
        // Characters which have been read from the input stream but have not
        // yet been committed to a token.
    Vector/*<SourceLoc>*/ look_ahead_locs;
        // One SourceLoc for each character in `look_ahead_chars`.
    size_t look_ahead_end;
        // The number of characters in `look_ahead_chars` which have been
        // tentatively committed to the current token. This can be reset to 0 if
        // the lexer needs to backtrack without emitting a token.
    size_t peeked_len;
        // The length of the last token peeked. This represents a prefix of
        // `look_ahead_chars`, and will be at most `look_ahead_end`. It is used
        // by Lexer_Commit to consume the last peeked token.
    bool eof;
        // Whether the input stream has reached the end of input.

    TokenTrie *tokens;
} Lexer;

PRIVATE void Lexer_Init(Lexer *lex, Blimp *blimp, Stream *input);
PRIVATE void Lexer_Destroy(Lexer *lex);

////////////////////////////////////////////////////////////////////////////////
// Parser
//

typedef struct {
    Vector/*<Production *>*/ productions;
    Vector/*<Vector<size_t>>*/ productions_for_non_terminals;
        // A map from non-terminals to the list of all productions for that
        // non-terminal.
    HashMap/*<Symbol *, NonTerminal>*/ non_terminals;
        // A map from symbols to the precedence level of the non-terminals they
        // represent.
    size_t num_terminals;
    Terminal eof_terminal;
    Vector/*<const char *>*/ terminal_strings;
        // A map from terminals to human-readable names. Used for error messages
        // and pretty-printing.
    Vector/*<const char *>*/ non_terminal_strings;
        // A map from non-terminals to human-readable names. Used for error
        // messages and pretty-printing.
    struct GrammarListener *listeners;
        // List of observers interested in finding out when the grammar changes.
    struct StateMachine *parse_table;
        // Cached, up-to-date parse table.
} Grammar;

#define ParseTree_Init BlimpParseTree_Init
PRIVATE void ParseTree_Destroy(ParseTree *tree);
PRIVATE Status ParseTree_Copy(
    Blimp *blimp, const ParseTree *from, ParseTree *to);
PRIVATE ParseTree *SubTree(const ParseTree *tree, size_t index);

typedef BlimpMacroHandler ProductionHandler;

static inline Blimp *Grammar_GetBlimp(const Grammar *grammar)
{
    return Vector_GetBlimp(&grammar->productions);
}

PRIVATE Status Grammar_Init(Blimp *blimp, Grammar *grammar, Terminal eof);
PRIVATE void Grammar_Destroy(Grammar *grammar);
PRIVATE Status Grammar_AddTerminal(Grammar *grammar, Terminal terminal);
PRIVATE Status Grammar_GetNonTerminal(
    Grammar *grammar, const Symbol *sym, NonTerminal *non_terminal);
PRIVATE Status Grammar_GetNonTerminalSymbol(
    Grammar *grammar, NonTerminal non_terminal, const Symbol **sym);
PRIVATE Status Grammar_AddRule(
    Grammar *grammar,
    NonTerminal non_terminal,
    size_t num_symbols,
    const GrammarSymbol *symbols,
    ProductionHandler handler,
    void *handler_arg);
PRIVATE void Grammar_SetTerminalString(
    Grammar *grammar, Terminal terminal, const char *string);
PRIVATE void Grammar_DumpVitals(FILE *file, Grammar *grammar);

PRIVATE Status Parse(
    Lexer *lex,
    Grammar *grammar,
    NonTerminal target,
    void *parser_state,
    ParseTree *parsed);
PRIVATE Status Reparse(
    const Vector/*<ParseTree>*/ *input,
    Grammar *grammar,
    NonTerminal target,
    void *parser_state,
    ParseTree *parsed);

#endif
