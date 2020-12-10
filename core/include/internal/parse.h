#ifndef BLIMP_PARSE_H
#define BLIMP_PARSE_H

#include "common.h"
#include "vector.h"

////////////////////////////////////////////////////////////////////////////////
// Lexer
//

PRIVATE void Stream_Delete(Stream *stream);

typedef enum {
    TOK_EOF,
    TOK_LBRACE,
    TOK_RBRACE,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_SEMI,
    TOK_MSG_NAME,
    TOK_MSG_THIS,
    TOK_SYMBOL,
    NUM_TOKEN_TYPES,
} TokenType;

#define TOK_INVALID NUM_TOKEN_TYPES

typedef struct {
    TokenType type;
    const Symbol *symbol;
    SourceRange range;
} Token;

PRIVATE const char *StringOfTokenType(TokenType t);
PRIVATE BlimpErrorCode UnexpectedTokenError(TokenType t);

typedef struct {
    Blimp *blimp;
    Stream *input;
    Token peek;
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
} Grammar;

typedef struct {
    Blimp *blimp;
    void *parser_state;
    void *arg;
    const SourceRange *range;
} ParserContext;

typedef Status(*ProductionHandler)(
    ParserContext *ctx, Expr **sub_exprs, Expr **result);

PRIVATE Status Grammar_Init(Blimp *blimp, Grammar *grammar, Terminal eof);
PRIVATE void Grammar_Destroy(Grammar *grammar);
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
    Lexer *lex, const Grammar *grammar, void *parser_state, Expr **expr);

#endif
