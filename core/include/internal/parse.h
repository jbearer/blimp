#ifndef BLIMP_PARSE_H
#define BLIMP_PARSE_H

#include "common.h"
#include "vector.h"

////////////////////////////////////////////////////////////////////////////////
// Lexer
//

PRIVATE void Stream_Delete(Stream *stream);

typedef enum {
    TOK_PRECEDENCE,
    TOK_MSG_NAME,
    TOK_MSG_THIS,
    TOK_SYMBOL,
    TOK_EOF,
    TOK_WHITESPACE,
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
PRIVATE BlimpErrorCode UnexpectedTokenError(TokenType t);

#define EOF_CHAR ((int)UCHAR_MAX + 1)
    // The EOF macro may be -1 (or some other negative value). It's convenient
    // to be able to to treat all possible values we might read from a stream as
    // indices into a lookup table, which does not work if `EOF` is negative.
    // Therefore, the lexer uses EOF_CHAR (which is one greater than the maximum
    // unsigned char value) to represent end-of-file, and internally translates
    // EOF to EOF_CHAR when reading from a stream.
#define NUM_CHARS (EOF_CHAR + 1)
    // One for each valid `char`, plus EOF_CHAR.

typedef struct {
    Blimp *blimp;
    size_t num_terminals;
    struct TrieNode *nodes[NUM_CHARS];
} TokenTrie;

PRIVATE Status TokenTrie_Init(Blimp *blimp, TokenTrie *trie);
PRIVATE void TokenTrie_Destroy(TokenTrie *trie);

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
    bool eof;
        // Whether the input stream has reached the end of input.

    Token peek;
    TokenTrie *tokens;
} Lexer;

PRIVATE void Lexer_Init(Lexer *lex, Blimp *blimp, Stream *input);
PRIVATE void Lexer_Destroy(Lexer *lex);
PRIVATE Status Lexer_Peek(Lexer *lex, Token *tok);
PRIVATE Status Lexer_Next(Lexer *lex, Token *tok);

////////////////////////////////////////////////////////////////////////////////
// Parser
//

typedef TokenType Terminal;

typedef size_t NonTerminal;
#define START_SYMBOL ((NonTerminal)0)

static inline NonTerminal PrecedenceNonTerminal(size_t precedence)
{
    return START_SYMBOL + 1 + precedence;
}

typedef struct {
    bool is_terminal;
    union {
        Terminal terminal;
        NonTerminal non_terminal;
    };
} GrammarSymbol;

typedef struct {
    Vector/*<Production *>*/ productions;
    Vector/*<Vector<size_t>>*/ productions_for_non_terminals;
        // A map from non-terminals to the list of all productions for that
        // non-terminal.
    size_t num_terminals;
    Terminal eof_terminal;
    Vector/*<const char *>*/ terminal_strings;
        // A map from terminals to human-readable names. Used for error messages
        // and pretty-printing.
    Vector/*<const char *>*/ non_terminal_strings;
        // A map from non-terminals to human-readable names. Used for error
        // messages and pretty-printing.
    struct GrammarListener *listeners;
} Grammar;

typedef struct {
    Blimp *blimp;
    void *parser_state;
    void *arg;
    const SourceRange *range;
} ParserContext;

typedef struct {
    GrammarSymbol symbol;
    Expr *parsed;
    Vector/*<ParseTree>*/ sub_trees;
} ParseTree;

PRIVATE void ParseTree_Destroy(ParseTree *tree);
PRIVATE Status ParseTree_Copy(const ParseTree *from, ParseTree *to);
PRIVATE Expr *ParsedExpr(const Vector/*<ParseTree>*/ *trees, size_t i);
PRIVATE const Symbol *ParsedToken(const Vector/*<ParseTree>*/ *trees, size_t i);

typedef Status(*ProductionHandler)(
    ParserContext *ctx, const Vector/*<ParseTree>*/ *sub_trees, Expr **parsed);

PRIVATE Status Grammar_Init(Blimp *blimp, Grammar *grammar, Terminal eof);
PRIVATE void Grammar_Destroy(Grammar *grammar);
PRIVATE Status Grammar_AddTerminal(Grammar *grammar, Terminal terminal);
PRIVATE Status Grammar_AddRule(
    Grammar *grammar,
    NonTerminal non_terminal,
    size_t num_symbols,
    const GrammarSymbol *symbols,
    ProductionHandler handler,
    void *handler_arg);
PRIVATE void Grammar_SetTerminalString(
    Grammar *grammar, Terminal terminal, const char *string);
PRIVATE void Grammar_SetNonTerminalString(
    Grammar *grammar, NonTerminal non_terminal, const char *string);
PRIVATE void Grammar_DumpVitals(FILE *file, const Grammar *grammar);

PRIVATE Status Parse(
    Lexer *lex, Grammar *grammar, void *parser_state, Expr **expr);
PRIVATE Status Reparse(
    const Vector/*<ParseTree>*/ *input,
    Grammar *grammar,
    void *parser_state,
    ParseTree *parsed);

#endif
