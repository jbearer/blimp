#include "internal/blimp.h"
#include "internal/expr.h"
#include "internal/grammar.h"

////////////////////////////////////////////////////////////////////////////////
// Lexer API
//

static Status GetToken(Blimp *blimp, const char *string, Token *tok)
{
    TRY(TokenTrie_GetToken(&blimp->tokens, string, tok));
    return BLIMP_OK;
}

static Status CreateToken(Blimp *blimp, const char *string, Token *tok)
{
    TRY(TokenTrie_InsertToken(&blimp->tokens, string, tok));
    TRY(Grammar_AddTerminal(&blimp->grammar, tok->type));
        // Let the grammar know about the (possibly) newly created terminal.
    Grammar_SetTerminalString(&blimp->grammar, tok->type, tok->symbol->name);
        // Set a readable name for the new token.
    return BLIMP_OK;
}

static Status CreateTerminal(
    Blimp *blimp, const char *string, Terminal *terminal)
{
    Token tok;
    TRY(CreateToken(blimp, string, &tok));
    *terminal = tok.type;
    return BLIMP_OK;
}

static Status PrecedenceSymbol(
    Blimp *blimp, size_t precedence, const Symbol **sym)
{
    // The symbol for a nonterminal with precedence `n` is `@n`.
    char buf[64] = "@";
    snprintf(&buf[1], sizeof(buf) - 1, "%zu", precedence);
    return Blimp_GetSymbol(blimp, buf, sym);
}

////////////////////////////////////////////////////////////////////////////////
// Parser API
//

// The public macro API is not directly compatible with the internal Grammar
// API, since some of the data types are simplified, and we do not expose all of
// the internal data types. Therefore, a BlimpMacroHandler can not be used
// directly as the handler for a grammar rule in Grammar_AddRule(). Instead,
// APIMacroHandler() must be used to translate between the arguments given to a
// Grammar macro handler and the arguments expected by a BlimpMacroHandler.
//
// APIMacroArg stores the API handler and its argument, which is called by
// APIMacroHandler after marshalling the arguments it receives from the parser.
typedef struct {
    BlimpMacroHandler handler;
    void *handler_arg;
} APIMacroArg;

static Status APIMacroHandler(
    ParserContext *ctx, const Vector/*<ParseTree>*/ *trees, Expr **parsed)
{
    APIMacroArg *arg = (APIMacroArg *)ctx->arg;

    // Convert `trees` from a list of ParseTrees to a list of Exprs.
    Vector/*<Expr *>*/ exprs;
    Vector_Init(ctx->blimp, &exprs, sizeof(Expr *), ExprDestructor);
    TRY(Vector_Reserve(&exprs, Vector_Length(trees)));
    for (ParseTree *tree = Vector_Begin(trees);
         tree != Vector_End(trees);
         tree = Vector_Next(trees, tree))
    {
        Expr *expr = BlimpExpr_Borrow(tree->parsed);
        CHECK(Vector_PushBack(&exprs, &expr));
    }

    // Call the user's handler.
    Status ret = arg->handler(
        ctx->blimp, Vector_Data(&exprs), arg->handler_arg, parsed);
    Vector_Destroy(&exprs);
    return ret;
}

Status Blimp_DefineMacro(
    Blimp *blimp,
    size_t precedence,
    BlimpGrammarSymbol *symbols,
    size_t num_symbols,
    BlimpMacroHandler handler,
    void *handler_arg)
{
    // Convert `symbols` from the public BlimpGrammarSymbol type to the internal
    // GrammarSymbol type.
    Vector/*<GrammarSymbol>*/ grammar_symbols;
    Vector_Init(blimp, &grammar_symbols, sizeof(GrammarSymbol), NULL);
    TRY(Vector_Reserve(&grammar_symbols, num_symbols));
    for (size_t i = 0; i < num_symbols; ++i) {
        GrammarSymbol sym;
        sym.is_terminal = symbols[i].is_terminal;
        if (sym.is_terminal) {
            // Convert the symbol for the token into the unique identifier by
            // which the lexer knows that token.
            if (CreateTerminal(blimp, symbols[i].symbol->name, &sym.terminal)
                != BLIMP_OK)
            {
                Vector_Destroy(&grammar_symbols);
                return Reraise(blimp);
            }
        } else {
            if (Grammar_GetNonTerminal(
                    &blimp->grammar, symbols[i].symbol, &sym.non_terminal)
                != BLIMP_OK)
            {
                Vector_Destroy(&grammar_symbols);
                return Reraise(blimp);
            }
        }

        CHECK(Vector_PushBack(&grammar_symbols, &sym));
            // This cannot fail since we reserved enough space in the vector
            // when we created it.
    }

    // Save info that the handler will need.
    APIMacroArg *arg;
    if (Malloc(blimp, sizeof(APIMacroArg), &arg) != BLIMP_OK) {
        Vector_Destroy(&grammar_symbols);
        return Reraise(blimp);
    }
    arg->handler = handler;
    arg->handler_arg = handler_arg;

    // Add a grammar rule coresponding to this macro.
    if (Grammar_AddRule(
            &blimp->grammar,
            precedence,
            num_symbols,
            Vector_Data(&grammar_symbols),
            APIMacroHandler,
            arg)
        != BLIMP_OK)
    {
        Free(blimp, &arg);
        Vector_Destroy(&grammar_symbols);
        return Reraise(blimp);
    }

    Vector_Destroy(&grammar_symbols);
    return BLIMP_OK;
}

////////////////////////////////////////////////////////////////////////////////
// Macros
//

typedef struct {
    const Symbol *non_terminal_symbol;
    Vector/*<ParseTree>*/ sub_trees;
} ParseTreeArg;

static void ParseTreeFinalizer(void *p)
{
    ParseTreeArg *arg = (ParseTreeArg *)p;
    Vector_Destroy(&arg->sub_trees);
    free(arg);
}

// Method for a parse tree extension object. This object represents a parse tree
// in a bl:mp program by mimicking the interface required of user-defined parse
// trees in macro definitions. That is, it receives a visitor and applies that
// visitor to each sub-tree, and it returns a symbol representing the
// non-terminal that resulted in this parse tree.
static Status ParseTreeMethod(
    Blimp *blimp,
    Object *context,
    Object *receiver,
    Object *message,
    Object **result)
{
    (void)context;

    ParseTreeArg *arg;
    CHECK(BlimpObject_ParseExtension(receiver, NULL, (void **)&arg));

    Vector/*<ParseTree>*/ *trees = &arg->sub_trees;
    const Symbol *precedence = arg->non_terminal_symbol;

    // Send each sub-tree to the `message`.
    for (ParseTree *tree = Vector_Begin(trees);
         tree != Vector_End(trees);
         tree = Vector_Next(trees, tree))
    {
        // Eliminate trivial reductions. If a parse tree only as one child, it
        // came from a trivial production `N -> β`, whose only function is to
        // relable `β` with the non-terminal `N`. Since this relabeling does not
        // change the structure of the tree, we do not expose it to the
        // programmer (which would make it much more annoying to handle the
        // parse tree in bl:mp code). Instead, we simply replace `N` with it's
        // sub-tree `β` for as long as `N` has only one sub-tree.
        ParseTree *curr = tree;
        while (Vector_Length(&curr->sub_trees) == 1) {
            curr = Vector_Index(&curr->sub_trees, 0);
        }

        // Get an object representing the sub-tree `curr`. This will be either a
        // symbol or a smaller parse tree extension object, depending on whether
        // `curr` is a terminal or a non-terminal with its own parse tree.
        Object *sub_tree;
        if (curr->symbol.is_terminal) {
            assert(Vector_Length(&curr->sub_trees) == 0);
            assert(curr->parsed->tag == EXPR_SYMBOL);
            TRY(BlimpObject_NewSymbol(blimp, curr->parsed->symbol, &sub_tree));
        } else {
            assert(Vector_Length(&curr->sub_trees) > 1);

            // Allocate an argument for ParseTreeMethod() for the sub-tree.
            ParseTreeArg *sub_arg;
            TRY(Malloc(blimp, sizeof(ParseTreeArg), &sub_arg));
            if (PrecedenceSymbol(
                    blimp,
                    curr->symbol.non_terminal,
                    &sub_arg->non_terminal_symbol
                ) != BLIMP_OK)
            {
                free(sub_arg);
                return Reraise(blimp);
            }
            // We give the sub-tree object a copy of the sub-trees of this parse
            // tree, since, while unlikely, the visitor to which we are going to
            // send the sub-tree may save a reference to it which lives longer
            // than the ParseTree `curr`.
            if (Vector_Copy(
                    &curr->sub_trees,
                    &sub_arg->sub_trees,
                    (CopyFunc)ParseTree_Copy
                ) != BLIMP_OK)
            {
                free(sub_arg);
                return Reraise(blimp);
            }

            // Create a parse tree extension object representing the sub-tree.
            if (BlimpObject_NewExtension(
                    blimp,
                    (Object *)blimp->global,
                    sub_arg,
                    ParseTreeMethod,
                    ParseTreeFinalizer,
                    &sub_tree
                ) != BLIMP_OK)
            {
                Vector_Destroy(&sub_arg->sub_trees);
                free(sub_arg);
                return Reraise(blimp);
            }
        }

        // Send the sub-tree to the `message`.
        if (Blimp_Send(blimp, (Object *)blimp->global, message, sub_tree, NULL)
                != BLIMP_OK)
        {
            BlimpObject_Release(sub_tree);
            return Reraise(blimp);
        }
        BlimpObject_Release(sub_tree);
    }

    return BlimpObject_NewSymbol(blimp, precedence, result);
}

static Status ParseObject(Blimp *blimp, Object *obj, ParseTree *tree);

// Method for a parse tree visitor extension object, which can be used to
// inspect a user-provided parse tree object (that is, an object which adheres
// to the parse tree protocol, where upon receiving a visitor it applies the
// visitor to all of its sub-trees). This object is a visitor which appends each
// of the visited sub-trees to a Vector.
static Status ParseTreeVisitor(
    Blimp *blimp,
    Object *context,
    Object *receiver,
    Object *message,
    Object **result)
{
    (void)context;

    Vector/*<ParseTree>*/ *trees;
    CHECK(BlimpObject_ParseExtension(receiver, NULL, (void **)&trees));

    ParseTree tree;
    TRY(ParseObject(blimp, message, &tree));
        // Recursively visit the sub-trees of the parse tree object we are
        // visiting and reparse those sub-trees into a single ParseTree.
        // Essentially, this converts the Object which represents a parse tree
        // into an actual ParseTree data structure.
    TRY(Vector_PushBack(trees, &tree));

    *result = BlimpObject_Borrow(receiver);
        // The return value here will most likely be ignored by the parse tree
        // object which invoked this visitor, but we need to return something,
        // so arbitrarily return the receiver.
    return BLIMP_OK;
}

// Extract a ParseTree from an Object which conforms to the parse tree protocol.
static Status ParseObject(Blimp *blimp, Object *obj, ParseTree *tree)
{
    if (Object_Type(obj) == OBJ_SYMBOL) {
        // Symbols represent terminals.
        Token tok;
        TRY(GetToken(blimp, ((const Symbol *)obj)->name, &tok));

        tree->symbol = (GrammarSymbol){.is_terminal=true, .terminal=tok.type};
        Vector_Init(
            blimp,
            &tree->sub_trees,
            sizeof(ParseTree),
            (Destructor)ParseTree_Destroy
        );
        TRY(BlimpExpr_NewSymbol(blimp, tok.symbol, &tree->parsed));

        return BLIMP_OK;
    }

    // Any non-symbol object represents a non-trivial parse tree, whose
    // sub-trees can be traversed by sending it a visitor. We will use a
    // ParseTreeVisitor to collect ParseTrees for each of the sub-trees in the
    // vector `trees`, and then we will parse that sequence of parse trees into
    // one larger tree.
    Vector/*<ParseTree>*/ trees;
    Vector_Init(
        blimp, &trees, sizeof(ParseTree), (Destructor)ParseTree_Destroy);
    Object *visitor;
    if (BlimpObject_NewExtension(
            blimp,
            (Object *)blimp->global,
            &trees,
            ParseTreeVisitor,
            (BlimpFinalizer)Vector_Destroy,
            &visitor
        ) != BLIMP_OK)
    {
        Vector_Destroy(&trees);
        return Reraise(blimp);
    }

    if (Blimp_Send(blimp, (Object *)blimp->global, obj, visitor, NULL)
            != BLIMP_OK)
    {
        BlimpObject_Release(visitor);
        return Reraise(blimp);
    }

    // Parse the sequence of sub-trees we collected into one larger tree.
    if (Reparse(&trees, &blimp->grammar, NULL, tree) != BLIMP_OK) {
        BlimpObject_Release(visitor);
        return Reraise(blimp);
    }
    BlimpObject_Release(visitor);
        // Release `visitor` _after_ the call to Reparse() above, as the
        // finalizer will destroy the `trees` vector.

    // The parser will always give us a parse tree with the lowest precedence
    // (1). However, since we are working with parse trees and not text, the
    // tree, being a self-contained parse tree, may as well have the highest
    // precedence possible, which means the user won't have to explicitly add
    // parentheses whenever they generate an already-self-contained parse tree.
    assert(!tree->symbol.is_terminal);
    assert(tree->symbol.non_terminal == 1);
    tree->symbol.non_terminal = NT_TermNoMsg;

    return BLIMP_OK;
}

typedef struct {
    Object *handler;
    const Symbol *non_terminal_symbol;
} MacroArg;

// Handler called when a macro is reduced.
static Status MacroHandler(
    ParserContext *ctx, const Vector/*<ParseTree>*/ *sub_trees, Expr **parsed)
{
    MacroArg *arg = (MacroArg *)ctx->arg;
    Object *handler = arg->handler;

    // Create the argument for a parse tree extension object which we will use
    // to convey the input `sub_trees` to the handler object.
    ParseTreeArg *input_arg;
    TRY(Malloc(ctx->blimp, sizeof(ParseTreeArg), &input_arg));
    input_arg->non_terminal_symbol = arg->non_terminal_symbol;
    // We give the parse tree object a copy of the input trees, since, while
    // unlikely, the macro handler may save a reference to the parse tree which
    // lives longer than the input `sub_trees`.
    if (Vector_Copy(sub_trees, &input_arg->sub_trees, (CopyFunc)ParseTree_Copy)
            != BLIMP_OK)
    {
        free(input_arg);
        return Reraise(ctx->blimp);
    }

    // Create the parse tree extension object representing the input trees.
    Object *input_tree;
    TRY(BlimpObject_NewExtension(
        ctx->blimp,
        (Object *)ctx->blimp->global,
        input_arg,
        ParseTreeMethod,
        ParseTreeFinalizer,
        &input_tree
    ));

    // Send the input tree to the macro handler, resulting in an Object
    // representing the output parse tree. This result object (`output_tree`)
    // must conform to the parse tree protocol, so we can recover a ParseTree
    // from it using ParseObject().
    Object *output_tree;
    if (Blimp_Send(
            ctx->blimp,
            (Object *)ctx->blimp->global,
            handler,
            input_tree,
            &output_tree
        ) != BLIMP_OK)
    {
        BlimpObject_Release(input_tree);
        return Reraise(ctx->blimp);
    }
    BlimpObject_Release(input_tree);

    // Parse the output Object.
    ParseTree tree;
    TRY(ParseObject(ctx->blimp, output_tree, &tree));
    *parsed = tree.parsed;

    return BLIMP_OK;
}

// Method for an extension object which visits a stream of symbols (of the kind
// required on the left-hand side of a macro definition), converts them to
// GrammarSymbols, and collects the results in a vector.
static Status SymbolVisitor(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)context;

    Vector/*<GrammarSymbol>*/ *symbols;
    TRY(BlimpObject_ParseExtension(receiver, NULL, (void **)&symbols));

    const Symbol *sym;
    TRY(BlimpObject_ParseSymbol(message, &sym));

    // Figure out what kind of token matches `sym`, creating a new keyword if
    // necessary.
    Token tok;
    TRY(CreateToken(blimp, sym->name, &tok));

    if (tok.type == TOK_PRECEDENCE) {
        // In the symbol list for a macro definition, a precedence token
        // represents a non-terminal.
        NonTerminal nt;
        TRY(Grammar_GetNonTerminal(&blimp->grammar, tok.symbol, &nt));

        TRY(Vector_PushBack(symbols, &(GrammarSymbol) {
            .is_terminal = false,
            .non_terminal = nt,
        }));
    } else {
        // Every other kind of token just represents a corresponding terminal.
        TRY(Vector_PushBack(symbols, &(GrammarSymbol) {
            .is_terminal = true,
            .terminal = tok.type,
        }));
    }

    *result = BlimpObject_Borrow(receiver);
    return BLIMP_OK;
}

// Handler for `Stmt @prec Term`
static Status PrecedenceHandler(
    ParserContext *ctx, const Vector/*<ParseTree>*/ *trees, Expr **parsed)
{
    Expr *production = ParsedExpr(trees, 0);
    Expr *handler    = ParsedExpr(trees, 2);

    // The expressions for the production and the handler have just now been
    // parsed; they have not been analyzed yet. Since we are going to evaluate
    // them now, at parse time, we need to resolve them.
    TRY(BlimpExpr_Resolve(ctx->blimp, production));
    TRY(BlimpExpr_Resolve(ctx->blimp, handler));

    // Get the precedence of the non-terminal for the macro we're defining (the
    // left-hand side of the production).
    const Symbol *nt_sym = ParsedToken(trees, 1);
    NonTerminal nt;
    TRY(Grammar_GetNonTerminal(&ctx->blimp->grammar, nt_sym, &nt));

    // Evaluate the production expression to get an Object which is a stream of
    // symbols (the right-hand side of the production).
    Object *production_obj;
    TRY(Blimp_Eval(
        ctx->blimp, production, (Object *)ctx->blimp->global, &production_obj));

    // Use a symbol visitor to extract a list of GrammarSymbols from
    // `production_obj`.
    Vector/*<GrammarSymbol>*/ symbols;
    Vector_Init(ctx->blimp, &symbols, sizeof(GrammarSymbol), NULL);
    Object *symbol_visitor;
    if (BlimpObject_NewExtension(
            ctx->blimp,
            (Object *)ctx->blimp->global,
            &symbols,
            SymbolVisitor,
            NULL,
            &symbol_visitor
        ) != BLIMP_OK)
    {
        Vector_Destroy(&symbols);
        BlimpObject_Release(production_obj);
        return Reraise(ctx->blimp);
    }

    if (Blimp_Send(
            ctx->blimp,
            (Object *)ctx->blimp->global,
            production_obj,
            symbol_visitor,
            NULL)
        != BLIMP_OK)
    {
        Vector_Destroy(&symbols);
        BlimpObject_Release(production_obj);
        return Reraise(ctx->blimp);
    }

    BlimpObject_Release(production_obj);
    BlimpObject_Release(symbol_visitor);

    // Evaluate the handler Object which will be attached to this macro, to run
    // whenever it is reduced.
    Object *handler_obj;
    if (Blimp_Eval(
            ctx->blimp,
            handler,
            (Object *)ctx->blimp->global,
            &handler_obj
        ) != BLIMP_OK)
    {
        Vector_Destroy(&symbols);
        return Reraise(ctx->blimp);
    }

    // Collect information needed by MacroHandler().
    MacroArg *handler_arg;
    if (Malloc(ctx->blimp, sizeof(MacroArg), &handler_arg) != BLIMP_OK) {
        Vector_Destroy(&symbols);
        BlimpObject_Release(handler_obj);
        return Reraise(ctx->blimp);
    }
    handler_arg->handler = handler_obj;
    handler_arg->non_terminal_symbol = nt_sym;

    // Add a new production which will be handled by `handler_obj`.
    if (Grammar_AddRule(
            &ctx->blimp->grammar,
            nt,
            Vector_Length(&symbols),
            Vector_Data(&symbols),
            MacroHandler,
            handler_arg)
        != BLIMP_OK)
    {
        Vector_Destroy(&symbols);
        BlimpObject_Release(handler_obj);
        free(handler_arg);
        return Reraise(ctx->blimp);
    }

    Vector_Destroy(&symbols);
    return BlimpExpr_NewSymbol(ctx->blimp, nt_sym, parsed);
}

////////////////////////////////////////////////////////////////////////////////
// Default grammar
//
// In which we define handlers for each production in the default bl:mp grammar
// (listed in grammar.h) and then define a function DefaultGrammar() which
// creates a Grammar object containing those productions.
//

// Use an existing sub-tree as the result of a reduction to a lower-precedence
// non-terminal. The index of the sub-tree to use, relative to the symbols
// matched by the production which triggered this reduction, is `ctx->arg`.
static Status IdentityHandler(
    ParserContext *ctx, const Vector/*<ParseTree>*/ *trees, Expr **result)
{
    *result = BlimpExpr_Borrow(ParsedExpr(trees, (size_t)ctx->arg));
    return BLIMP_OK;
}

// Handler for `Expr -> Stmt ; Expr`
static Status SequenceHandler(
    ParserContext *ctx, const Vector/*<ParseTree>*/ *trees, Expr **result)
{
    (void)ctx;

    // The sub-statement may already be a sequence, so combining it with the
    // sub-expression amounts to appending two linked lists.
    ParsedExpr(trees, 0)->last->next = BlimpExpr_Borrow(ParsedExpr(trees, 2));
    ParsedExpr(trees, 0)->last = ParsedExpr(trees, 2)->last;
    *result = BlimpExpr_Borrow(ParsedExpr(trees, 0));
    return BLIMP_OK;
}

// Handler for `Stmt -> Stmt -> Expr`
static Status SendHandler(
    ParserContext *ctx, const Vector/*<ParseTree>*/ *trees, Expr **result)
{
    return BlimpExpr_NewSend(
        ctx->blimp, ParsedExpr(trees, 0), ParsedExpr(trees, 1), result);
}

// Handler for `Term -> {^msg Expr}` and `Term -> {Expr}`. `ctx->arg` should be
// non-zero if an explicit message name for the block is provided (first case)
// and zero otherwise (second case).
static Status BlockHandler(
    ParserContext *ctx, const Vector/*<ParseTree>*/ *trees, Expr **result)
{
    bool has_msg_name = (bool)ctx->arg;

    Expr *body = ParsedExpr(trees, 1);

    const Symbol *msg_name = NULL;
    if (has_msg_name) {
        const Symbol *msg_tok = ParsedToken(trees, 1);
        assert(msg_tok->name[0] == '^');
        assert(msg_tok->name[1] != '\0');
        TRY(Blimp_GetSymbol(ctx->blimp, &msg_tok->name[1], &msg_name));
        body = ParsedExpr(trees, 2);
            // The message name symbol offsets the index of the body symbol by
            // 1.
    }

    return BlimpExpr_NewBlock(ctx->blimp, msg_name, body, result);
}

// Handler for `Term -> symbol`
static Status SymbolHandler(
    ParserContext *ctx, const Vector/*<ParseTree>*/ *trees, Expr **result)
{
    const Symbol *tok = ParsedToken(trees, 0);

    // The symbol name may be escaped: enclosed by backticks and possibly
    // containing backslash escape sequences. Make a copy so we can transform it
    // in place to an unescaped version.
    char *escaped;
    TRY(Strdup(ctx->blimp, tok->name, &escaped));
    char *unescaped = escaped;

    // If there are enclosing back-ticks, remove the first one.
    if (*unescaped == '`') {
        ++unescaped;
    }

    // Walk the string with a read pointer and a write pointer (which is always
    // equal to the read pointer or lagging behind it). Remove each unescaped
    // backslash, and advance for all non-backslash characters.
    char *write = unescaped;
    for (char *read = unescaped; *read != '\0'; ++read) {
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

    // If there are enclosing backticks, remove the closing one.
    if (*(write - 1) == '`') {
        *(write - 1) = '\0';
    }

    // Get a Symbol for the unescaped string we just created.
    const Symbol *sym;
    if (Blimp_GetSymbol(ctx->blimp, unescaped, &sym) != BLIMP_OK) {
        free(escaped);
        return Reraise(ctx->blimp);
    }
    free(escaped);

    return BlimpExpr_NewSymbol(ctx->blimp, sym, result);
}

// Handler for `Term -> ^msg`
static Status MsgHandler(
    ParserContext *ctx, const Vector/*<ParseTree>*/ *trees, Expr **result)
{
    const Symbol *tok = ParsedToken(trees, 0);
    assert(tok->name[0] == '^');
    assert(tok->name[1] != '\0');

    const Symbol *sym;
    TRY(Blimp_GetSymbol(ctx->blimp, &tok->name[1], &sym));
    return BlimpExpr_NewMsgName(ctx->blimp, sym, result);
}

// Handler for `Term -> ^`
static Status MsgThisHandler(
    ParserContext *ctx, const Vector/*<ParseTree>*/ *trees, Expr **result)
{
    (void)trees;

    return BlimpExpr_NewMsgIndex(ctx->blimp, 0, result);
}

static Status RegisterNonTerminal(
    Grammar *grammar, NonTerminal nt, const char *name)
{
    const Symbol *sym;
    TRY(Blimp_GetSymbol(Grammar_GetBlimp(grammar), name, &sym));
    TRY(Grammar_SetNonTerminalSymbol(grammar, nt, sym));
    return BLIMP_OK;
}

// You can't pass an array literal as an argument to a macro, because commas in
// the macro argument list which are not intended to separate arguments must be
// enclosed in parentheses, but enclosing a brace-enclosed array literal in
// parentheses does not work for constructs like `(Type[]){elems}` (because
// `(Type[])({elems})` is not allowed). Therefore, we require macro callers to
// pass a parentheses-enclosed comma-separated list instead, and we use the
// ARRAY() macro to turn the parentheses into braces.
#define UNPAREN(...)__VA_ARGS__
#define APPLY(x) x
#define ARRAY(ELEMS) {APPLY(UNPAREN ELEMS)}

#define ADD_GRAMMAR_RULE(GRAMMAR, NT, SYMBOLS, HANDLER, ARG) \
    Grammar_AddRule( \
        GRAMMAR, \
        CONCAT(NT_,NT), \
        sizeof((GrammarSymbol[])ARRAY(SYMBOLS))/sizeof(GrammarSymbol), \
        (GrammarSymbol[])ARRAY(SYMBOLS), HANDLER, ARG)

#define CONCAT(x, y) x ## y
#define T(TERMINAL) {.is_terminal = true, .terminal = CONCAT(TOK_,TERMINAL)}
#define NT(NON_TERMINAL) {.is_terminal = false, .non_terminal = CONCAT(NT_,NON_TERMINAL)}

Status DefaultGrammar(Blimp *blimp, Grammar *grammar)
{
    TRY(Grammar_Init(blimp, grammar, TOK_EOF));

    // Add terminals used by the default productions to the lexer:
    Terminal TOK_SEMI, TOK_LBRACE, TOK_RBRACE, TOK_LPAREN, TOK_RPAREN;
    TRY(CreateTerminal(blimp, ";", &TOK_SEMI));
    TRY(CreateTerminal(blimp, "{", &TOK_LBRACE));
    TRY(CreateTerminal(blimp, "}", &TOK_RBRACE));
    TRY(CreateTerminal(blimp, "(", &TOK_LPAREN));
    TRY(CreateTerminal(blimp, ")", &TOK_RPAREN));

    // And here are the productions:

    // Start = Expr
    TRY(ADD_GRAMMAR_RULE(grammar, Start, (NT(Expr)),
        IdentityHandler, NULL));

    // Expr[NoMsg] = Stmt[NoMsg] Semi Expr
    TRY(ADD_GRAMMAR_RULE(grammar, Expr, (NT(Stmt), NT(Semi), NT(Expr)),
        SequenceHandler, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, ExprNoMsg, (NT(StmtNoMsg), NT(Semi), NT(Expr)),
        SequenceHandler, NULL));
    //      \ Stmt[NoMsg]
    TRY(ADD_GRAMMAR_RULE(grammar, Expr, (NT(Stmt)),
        IdentityHandler, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, ExprNoMsg, (NT(StmtNoMsg)),
        IdentityHandler, NULL));

    // Stmt[NoMsg] = Stmt[NoMsg] Term
    TRY(ADD_GRAMMAR_RULE(grammar, Stmt, (NT(Stmt), NT(Term)),
        SendHandler, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, StmtNoMsg, (NT(StmtNoMsg), NT(Term)),
        SendHandler, NULL));
    //      \ Stmt[NoMsg] @prec Term
    TRY(ADD_GRAMMAR_RULE(grammar, Stmt, (NT(Stmt), T(PRECEDENCE), NT(Term)),
        PrecedenceHandler, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, StmtNoMsg, (NT(StmtNoMsg), T(PRECEDENCE), NT(Term)),
        PrecedenceHandler, NULL));
    //      \ term
    TRY(ADD_GRAMMAR_RULE(grammar, Stmt, (NT(Term)),
        IdentityHandler, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, StmtNoMsg, (NT(TermNoMsg)),
        IdentityHandler, NULL));

    // Semi = ";"
    TRY(ADD_GRAMMAR_RULE(grammar, Semi, (T(SEMI)),
        IdentityHandler, NULL));

    // Term = TermNoMsg \ Msg
    TRY(ADD_GRAMMAR_RULE(grammar, Term, (NT(TermNoMsg)),
        IdentityHandler, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, Term, (T(MSG_NAME)),
        MsgHandler, NULL));

    // TermNoMsg = "(" expr ")"
    TRY(ADD_GRAMMAR_RULE(grammar, TermNoMsg, (T(LPAREN), NT(Expr), T(RPAREN)),
        IdentityHandler, (void *)1));
    //      \ "{" [msg] expr "}"
    TRY(ADD_GRAMMAR_RULE(grammar, TermNoMsg, (T(LBRACE), T(MSG_NAME), NT(Expr), T(RBRACE)),
        BlockHandler, (void *)1));
    TRY(ADD_GRAMMAR_RULE(grammar, TermNoMsg, (T(LBRACE), NT(ExprNoMsg), T(RBRACE)),
        BlockHandler, (void *)0));
    //      \ symbol
    TRY(ADD_GRAMMAR_RULE(grammar, TermNoMsg, (T(SYMBOL)),
        SymbolHandler, NULL));
    //      \ "^"
    TRY(ADD_GRAMMAR_RULE(grammar, TermNoMsg, (T(MSG_THIS)),
        MsgThisHandler, NULL));

    // Give the parser readable names for the built-in terminals (user-defined
    // terminals will be named by the string used to create the terminal; see
    // CreateTerminal()).
    for (TokenType t = 0; t < NUM_BUILT_IN_TOKENS; ++t) {
        Grammar_SetTerminalString(grammar, t, StringOfTokenType(t));
    }

    // Register symbolic names for all of the non-terminals.
    TRY(RegisterNonTerminal(grammar, NT_Start,     "@0"));
    TRY(RegisterNonTerminal(grammar, NT_Expr,      "@1"));
    TRY(RegisterNonTerminal(grammar, NT_ExprNoMsg, "@2"));
    TRY(RegisterNonTerminal(grammar, NT_Stmt,      "@3"));
    TRY(RegisterNonTerminal(grammar, NT_StmtNoMsg, "@4"));
    TRY(RegisterNonTerminal(grammar, NT_Semi,      "@5"));
    TRY(RegisterNonTerminal(grammar, NT_Term,      "@6"));
    TRY(RegisterNonTerminal(grammar, NT_TermNoMsg, "@7"));

    return BLIMP_OK;
}
