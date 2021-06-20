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

static Status APIMacroHandler(ParserContext *ctx, ParseTree *tree)
{
    APIMacroArg *arg = (APIMacroArg *)ctx->arg;

    // Convert the parsed sub-trees from ParseTrees to a list of Exprs.
    Vector/*<Expr *>*/ exprs;
    Vector_Init(ctx->blimp, &exprs, sizeof(Expr *), ExprDestructor);
    TRY(Vector_Reserve(&exprs, Vector_Length(&tree->sub_trees)));
    for (ParseTree *sub_tree = Vector_Begin(&tree->sub_trees);
         sub_tree != Vector_End(&tree->sub_trees);
         sub_tree = Vector_Next(&tree->sub_trees, sub_tree))
    {
        Expr *expr = BlimpExpr_Borrow(sub_tree->parsed);
        CHECK(Vector_PushBack(&exprs, &expr));
    }

    // Call the user's handler.
    Status ret = arg->handler(
        ctx->blimp, Vector_Data(&exprs), arg->handler_arg, &tree->parsed);
    Vector_Destroy(&exprs);
    return ret;
}

Status Blimp_DefineMacro(
    Blimp *blimp,
    const Symbol *non_terminal,
    BlimpGrammarSymbol *symbols,
    size_t num_symbols,
    BlimpMacroHandler handler,
    void *handler_arg)
{
    NonTerminal nt;
    TRY(Grammar_GetNonTerminal(&blimp->grammar, non_terminal, &nt));

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
            nt,
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

static Status GetNonTerminal(
    Grammar *grammar, const char *name, NonTerminal *nt)
{
    const Symbol *sym;
    TRY(Blimp_GetSymbol(Grammar_GetBlimp(grammar), name, &sym));
    TRY(Grammar_GetNonTerminal(grammar, sym, nt));
    return BLIMP_OK;
}

typedef struct {
    const Symbol *non_terminal_symbol;
    Vector/*<ParseTree>*/ sub_trees;
    Expr *parsed;
} ParseTreeArg;

static void ParseTreeFinalizer(void *p)
{
    ParseTreeArg *arg = (ParseTreeArg *)p;
    Vector_Destroy(&arg->sub_trees);
    Blimp_FreeExpr(arg->parsed);
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

    // Send each sub-tree to the `message`.
    for (ParseTree *tree = Vector_Begin(trees);
         tree != Vector_End(trees);
         tree = Vector_Next(trees, tree))
    {
        // Get an object representing the sub-tree `tree`. This will be either a
        // symbol or a smaller parse tree extension object, depending on whether
        // `tree` is a terminal or a non-terminal with its own parse tree.
        Object *sub_tree;
        if (tree->symbol.is_terminal) {
            assert(Vector_Length(&tree->sub_trees) == 0);
            assert(tree->parsed->tag == EXPR_SYMBOL);
            TRY(BlimpObject_NewSymbol(blimp, tree->parsed->symbol, &sub_tree));
        } else {
            // Allocate an argument for ParseTreeMethod() for the sub-tree.
            ParseTreeArg *sub_arg;
            TRY(Malloc(blimp, sizeof(ParseTreeArg), &sub_arg));
            sub_arg->parsed = BlimpExpr_Borrow(tree->parsed);
            if (Grammar_GetNonTerminalSymbol(
                    &blimp->grammar,
                    tree->symbol.non_terminal,
                    &sub_arg->non_terminal_symbol
                ) != BLIMP_OK)
            {
                free(sub_arg);
                return Reraise(blimp);
            }
            // We give the sub-tree object a copy of the sub-trees of this parse
            // tree, since, while unlikely, the visitor to which we are going to
            // send the sub-tree may save a reference to it which lives longer
            // than the ParseTree `tree`.
            if (Vector_Copy(
                    &tree->sub_trees,
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

    return BlimpObject_NewSymbol(blimp, arg->non_terminal_symbol, result);
}

static Status ObjectToParseTree(Blimp *blimp, Object *obj, ParseTree *tree);

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
    TRY(ObjectToParseTree(blimp, message, &tree));
        // Recursively visit the sub-trees of the parse tree object we are
        // visiting and convert those sub-trees into a single ParseTree.
        // Essentially, this converts the Object which represents a parse tree
        // into an actual ParseTree data structure.
    TRY(Vector_PushBack(trees, &tree));

    *result = BlimpObject_Borrow(receiver);
        // The return value here will most likely be ignored by the parse tree
        // object which invoked this visitor, but we need to return something,
        // so arbitrarily return the receiver.
    return BLIMP_OK;
}

static Status ParseTree_ReconstructExpr(Blimp *blimp, ParseTree *tree);

// Extract a ParseTree from an Object which conforms to the parse tree protocol.
// This function does not reparse the Object. Instead, it trusts that the object
// represents a valid parse tree.
static Status ObjectToParseTree(Blimp *blimp, Object *obj, ParseTree *tree)
{
    ParseTreeArg *arg;
    BlimpMethod method;
    if (BlimpObject_ParseExtension(obj, &method, (void **)&arg) == BLIMP_OK &&
        method == ParseTreeMethod)
    {
        // As an optimization, if the object is a ParseTree extension object,
        // just extract its contents directly, rather than recursively
        // traversing the parse tree and rebuilding it by sending a visitor to
        // the object.
        //
        // Note that bl:mp semantics describe the object being visited in order
        // to interpret it as a parse tree, but for built-in parse tree
        // extensions, this optimization is transparent because
        //  1. Parse tree extensions are always pure; visiting them has no side-
        //     effects.
        //  2. Parse tree extensions conform to the parse tree protocol in the
        //     expected way, so that visiting them and rebuilding the parse tree
        //     should always yield the original parse tree.
        tree->symbol.is_terminal = false;
        if (Grammar_GetNonTerminal(
                &blimp->grammar,
                arg->non_terminal_symbol,
                &tree->symbol.non_terminal) != BLIMP_OK)
        {
            goto error;
        }
        if (Vector_Copy(
                &arg->sub_trees, &tree->sub_trees, (CopyFunc)ParseTree_Copy)
            != BLIMP_OK)
        {
            goto error;
        }
        tree->parsed = BlimpExpr_Borrow(arg->parsed);
        return BLIMP_OK;
    }

    if (Object_Type(obj) == OBJ_SYMBOL) {
        // Symbols represent terminals and vice versa.
        Token tok;
        if (GetToken(blimp, ((const Symbol *)obj)->name, &tok) != BLIMP_OK) {
            goto error;
        }

        tree->symbol = (GrammarSymbol){.is_terminal=true, .terminal=tok.type};
        Vector_Init(
            blimp,
            &tree->sub_trees,
            sizeof(ParseTree),
            (Destructor)ParseTree_Destroy
        );
        if (BlimpExpr_NewSymbol(blimp, tok.symbol, &tree->parsed) != BLIMP_OK) {
            goto error;
        }

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
        goto error;
    }

    // Send the visitor to the object to collect its sub-trees, and get the
    // non-terminal to parse, which is the return value of the parse tree
    // object.
    const Symbol *nt;
    if (Blimp_SendAndParseSymbol(
            blimp, (Object *)blimp->global, obj, visitor, &nt)
        != BLIMP_OK)
    {
        BlimpObject_Release(visitor);
        goto error;
    }

    // Collect the parsed sub-trees into a new parse tree.
    tree->symbol.is_terminal = false;
    if (Grammar_GetNonTerminal(&blimp->grammar, nt, &tree->symbol.non_terminal)
            != BLIMP_OK)
    {
        BlimpObject_Release(visitor);
        goto error;
    }
    if (Vector_Move(&trees, &tree->sub_trees) != BLIMP_OK) {
        BlimpObject_Release(visitor);
        goto error;
    }
    BlimpObject_Release(visitor);
        // Release `visitor` _after_ moving the `trees` vector to
        // `tree->sub_trees`, as the finalizer will destroy the `trees` vector.

    // Convert the parse tree to an expression, filling in `tree->parsed`.
    if (ParseTree_ReconstructExpr(blimp, tree) != BLIMP_OK) {
        goto error;
    }

    return BLIMP_OK;

error:
    // Make sure `tree` is a valid parse tree even if we failed.
    tree->parsed = NULL;
    tree->symbol = (GrammarSymbol){.is_terminal=true, .terminal=TOK_INVALID};
    Vector_Init(
        blimp,
        &tree->sub_trees,
        sizeof(ParseTree),
        (Destructor)ParseTree_Destroy
    );
    return Reraise(blimp);
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
        goto error;
    }

    // Send the visitor to the object to collect its sub-trees, and get the
    // non-terminal to parse, which is the return value of the parse tree
    // object.
    const Symbol *nt_symbol;
    if (Blimp_SendAndParseSymbol(
            blimp, (Object *)blimp->global, obj, visitor, &nt_symbol)
        != BLIMP_OK)
    {
        BlimpObject_Release(visitor);
        goto error;
    }

    // Parse the sequence of sub-trees we collected into one larger tree.
    NonTerminal nt;
    if (Grammar_GetNonTerminal(&blimp->grammar, nt_symbol, &nt) != BLIMP_OK) {
        BlimpObject_Release(visitor);
        goto error;
    }
    if (Reparse(&trees, &blimp->grammar, nt, NULL, tree) != BLIMP_OK) {
        BlimpObject_Release(visitor);
        goto error;
    }
    BlimpObject_Release(visitor);
        // Release `visitor` _after_ the call to Reparse() above, as the
        // finalizer will destroy the `trees` vector.

    assert(!tree->symbol.is_terminal);
    assert(tree->symbol.non_terminal == nt);
        // We should have got back a tree with the precedence we were trying to
        // parse.

    return BLIMP_OK;

error:
    // Make sure `tree` is a valid parse tree even if we failed.
    tree->parsed = NULL;
    tree->symbol = (GrammarSymbol){.is_terminal=true, .terminal=TOK_INVALID};
    Vector_Init(
        blimp,
        &tree->sub_trees,
        sizeof(ParseTree),
        (Destructor)ParseTree_Destroy
    );
    return Reraise(blimp);
}

typedef struct {
    Object *handler;
    const Symbol *non_terminal_symbol;
} MacroArg;

// Handler called when a macro is reduced.
static Status MacroHandler(ParserContext *ctx, ParseTree *tree)
{
    MacroArg *arg = (MacroArg *)ctx->arg;
    Object *handler = arg->handler;

    // Create the argument for a parse tree extension object which we will use
    // to convey the input sub-trees to the handler object.
    ParseTreeArg *input_arg;
    TRY(Malloc(ctx->blimp, sizeof(ParseTreeArg), &input_arg));
    input_arg->parsed = NULL;
    input_arg->non_terminal_symbol = arg->non_terminal_symbol;
    // We give the parse tree object a copy of the input trees, since, while
    // unlikely, the macro handler may save a reference to the parse tree which
    // lives longer than the input sub-trees.
    if (Vector_Copy(
            &tree->sub_trees, &input_arg->sub_trees, (CopyFunc)ParseTree_Copy)
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
    ParseTree_Destroy(tree);
    TRY(ParseObject(ctx->blimp, output_tree, tree));

    // The symbol of the resulting parse tree will be whatever the output of the
    // macro handler reduced to. But the invocation of the macro is supposed to
    // reduce to the non-terminal with which the macro is defined. As a last
    // step, fix the symbol of the resulting parse tree.
    tree->symbol.is_terminal = false;
    TRY(Grammar_GetNonTerminal(
        &ctx->blimp->grammar,
        arg->non_terminal_symbol,
        &tree->symbol.non_terminal));

    return BLIMP_OK;
}

static Status IgnorerMethod(
    Blimp *blimp,
    Object *context,
    Object *receiver,
    Object *message,
    Object **result)
{
    (void)blimp;
    (void)context;
    (void)message;

    *result = BlimpObject_Borrow(receiver);
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

    if (Object_Type(message) == OBJ_SYMBOL) {
        // If the grammar symbol is also a symbol object, it represents a
        // terminal.
        const Symbol *sym = (const Symbol *)message;

        // Figure out what kind of token matches `sym`, creating a new keyword
        // if necessary.
        Token tok;
        TRY(CreateToken(blimp, sym->name, &tok));

        TRY(Vector_PushBack(symbols, &(GrammarSymbol) {
            .is_terminal = true,
            .terminal = tok.type,
        }));
    } else {
        // Otherwise, we treat the object as a parse tree representing a non-
        // terminal. We only care about the precedence of the parse tree (it
        // will match any non-terminal with the same precedence) so we send it a
        // message to get its return value, which is a symbol representing the
        // non-terminal.

        // Create an extension object to send to the parse tree. The parse tree
        // will try to output sub-trees by sending them to the extension; we'll
        // use an extension object that just ignores them, since we only care
        // about the return value of the parse tree.
        Object *ignorer;
        TRY(BlimpObject_NewExtension(
            blimp,
            (Object *)blimp->global,
            NULL,
            IgnorerMethod,
            NULL,
            &ignorer
        ));

        // Send the ignorer to the parse tree and get the return value.
        const Symbol *nt_sym;
        if (Blimp_SendAndParseSymbol(
                blimp, (Object *)blimp->global, message, ignorer, &nt_sym)
            != BLIMP_OK)
        {
            BlimpObject_Release(ignorer);
            return Reraise(blimp);
        }
        BlimpObject_Release(ignorer);

        // Convert the symbol to the internal representation of a non-terminal.
        NonTerminal nt;
        TRY(Grammar_GetNonTerminal(&blimp->grammar, nt_sym, &nt));

        TRY(Vector_PushBack(symbols, &(GrammarSymbol) {
            .is_terminal = false,
            .non_terminal = nt,
        }));
    }

    *result = BlimpObject_Borrow(receiver);
    return BLIMP_OK;
}

// Handler for `\> Term Term Term`
static Status PrecedenceHandler(ParserContext *ctx, ParseTree *tree)
{
    Expr *production = SubExpr(tree, 1);
    Expr *handler    = SubExpr(tree, 2);

    // The expressions for the production and the handler have just now been
    // parsed; they have not been analyzed yet. Since we are going to evaluate
    // them now, at parse time, we need to resolve them.
    TRY(BlimpExpr_Resolve(ctx->blimp, production));
    TRY(BlimpExpr_Resolve(ctx->blimp, handler));

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

    // Send the visitor to the production parse tree and get the return value,
    // which is the non-terminal of the production.
    const Symbol *nt_sym;
    if (Blimp_SendAndParseSymbol(
            ctx->blimp,
            (Object *)ctx->blimp->global,
            production_obj,
            symbol_visitor,
            &nt_sym)
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
    NonTerminal nt;
    TRY(Grammar_GetNonTerminal(&ctx->blimp->grammar, nt_sym, &nt));
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
    return BlimpExpr_NewSymbol(ctx->blimp, nt_sym, &tree->parsed);
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
static Status IdentityHandler(ParserContext *ctx, ParseTree *tree)
{
    tree->parsed = BlimpExpr_Borrow(SubExpr(tree, (size_t)ctx->arg));
    return BLIMP_OK;
}

// Handler for `Expr -> Stmt ; Expr`
static Status SequenceHandler(ParserContext *ctx, ParseTree *tree)
{
    (void)ctx;

    // The sub-statement may already be a sequence, so combining it with the
    // sub-expression amounts to appending two linked lists.
    SubExpr(tree, 0)->last->next = BlimpExpr_Borrow(SubExpr(tree, 2));
    SubExpr(tree, 0)->last = SubExpr(tree, 2)->last;
    tree->parsed = BlimpExpr_Borrow(SubExpr(tree, 0));
    return BLIMP_OK;
}

// Handler for `Stmt -> Stmt -> Expr`
static Status SendHandler(ParserContext *ctx, ParseTree *tree)
{
    return BlimpExpr_NewSend(
        ctx->blimp,
        SubExpr(tree, 0),
        SubExpr(tree, 1),
        &tree->parsed
    );
}

// Handler for `Term -> {^msg Expr}` and `Term -> {Expr}`. `ctx->arg` should be
// non-zero if an explicit message name for the block is provided (first case)
// and zero otherwise (second case).
static Status BlockHandler(ParserContext *ctx, ParseTree *tree)
{
    bool has_msg_name = (bool)ctx->arg;

    Expr *body = SubExpr(tree, 1);

    const Symbol *msg_name = NULL;
    if (has_msg_name) {
        const Symbol *msg_tok = SubToken(tree, 1);
        assert(msg_tok->name[0] == '^');
        assert(msg_tok->name[1] != '\0');
        TRY(Blimp_GetSymbol(ctx->blimp, &msg_tok->name[1], &msg_name));
        body = SubExpr(tree, 2);
            // The message name symbol offsets the index of the body symbol by
            // 1.
    }

    return BlimpExpr_NewBlock(ctx->blimp, msg_name, body, &tree->parsed);
}

// Handler for `Term -> ^msg`
static Status MsgHandler(ParserContext *ctx, ParseTree *tree)
{
    const Symbol *tok = SubToken(tree, 0);
    assert(tok->name[0] == '^');
    assert(tok->name[1] != '\0');

    const Symbol *sym;
    TRY(Blimp_GetSymbol(ctx->blimp, &tok->name[1], &sym));
    return BlimpExpr_NewMsgName(ctx->blimp, sym, &tree->parsed);
}

// Handler for `Term -> ^`
static Status MsgThisHandler(ParserContext *ctx, ParseTree *tree)
{
    return BlimpExpr_NewMsgIndex(ctx->blimp, 0, &tree->parsed);
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
    Terminal TOK_SEMI, TOK_LBRACE, TOK_RBRACE, TOK_LPAREN, TOK_RPAREN, TOK_MACRO;
    TRY(CreateTerminal(blimp, ";", &TOK_SEMI));
    TRY(CreateTerminal(blimp, "{", &TOK_LBRACE));
    TRY(CreateTerminal(blimp, "}", &TOK_RBRACE));
    TRY(CreateTerminal(blimp, "(", &TOK_LPAREN));
    TRY(CreateTerminal(blimp, ")", &TOK_RPAREN));
    TRY(CreateTerminal(blimp, "\\>", &TOK_MACRO));

    // Add non-terminals used by the default productions to the parser, in order
    // of increasing precedence:
    NonTerminal NT_Expr, NT_ExprNoMsg, NT_Stmt, NT_StmtNoMsg, NT_Semi, NT_Term,
        NT_TermNoMsg;
    TRY(GetNonTerminal(grammar,"1", &NT_Expr));
    TRY(GetNonTerminal(grammar,"2", &NT_ExprNoMsg));
    TRY(GetNonTerminal(grammar,"3", &NT_Stmt));
    TRY(GetNonTerminal(grammar,"4", &NT_StmtNoMsg));
    TRY(GetNonTerminal(grammar,"5", &NT_Semi));
    TRY(GetNonTerminal(grammar,"6", &NT_Term));
    TRY(GetNonTerminal(grammar,"7", &NT_TermNoMsg));

    // And here are the productions:

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
    //      \> Term Term
    TRY(ADD_GRAMMAR_RULE(grammar, Stmt, (T(MACRO), NT(Term), NT(Term)),
        PrecedenceHandler, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, StmtNoMsg, (T(MACRO), NT(Term), NT(Term)),
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
        IdentityHandler, NULL));
    //      \ "^"
    TRY(ADD_GRAMMAR_RULE(grammar, TermNoMsg, (T(MSG_THIS)),
        MsgThisHandler, NULL));

    // Give the parser readable names for the built-in terminals (user-defined
    // terminals will be named by the string used to create the terminal; see
    // CreateTerminal()).
    for (TokenType t = 0; t < NUM_BUILT_IN_TOKENS; ++t) {
        Grammar_SetTerminalString(grammar, t, StringOfTokenType(t));
    }

    return BLIMP_OK;
}

static inline bool SubTreeIsTerminal(
    const ParseTree *tree, size_t index, Terminal t)
{
    ParseTree *sub_tree = (ParseTree *)Vector_Index(&tree->sub_trees, index);
    return sub_tree->symbol.is_terminal && sub_tree->symbol.terminal == t;
}

// Convert parse tree into an expression. In bl:mp semantics, this step is a
// no-op because parse trees in the basic grammar, after macro expansions; are
// already expressions. That is, a parse tree made up of only primitive
// productions is the same S-expression as one of the bl:mp expression forms.
//
// In our implementation of the semantics, though, ParseTree data structures
// represent S-expressions while the Expr type is a more structured
// representation with extra metadata. So we need to match the form of the parse
// tree against one of the primitive productions and construct the appropriate
// Expr, which would have been returned by the reduction handler for that
// production.
//
// Right now, this logic is a bit complicated, as it is basically an ad-hoc
// parser for the built-in bl:mp grammar. However, the long-term goal is to
// simplify the built-in grammar so that it contains only one production for
// each primitive syntactic construct (symbol and block literals, macros, and
// sends) and so that each production starts with a prefix terminal which
// indicates the production being used. Then, this logic will simply amount to
// checking the prefix terminal, validating the rest of the tree, and calling
// the appropriate handler.
static Status ParseTree_ReconstructExpr(Blimp *blimp, ParseTree *tree)
{
    Terminal TOK_SEMI, TOK_LBRACE, TOK_RBRACE, TOK_LPAREN, TOK_RPAREN;
    TRY(CreateTerminal(blimp, ";", &TOK_SEMI));
    TRY(CreateTerminal(blimp, "{", &TOK_LBRACE));
    TRY(CreateTerminal(blimp, "}", &TOK_RBRACE));
    TRY(CreateTerminal(blimp, "(", &TOK_LPAREN));
    TRY(CreateTerminal(blimp, ")", &TOK_RPAREN));

    ParserContext ctx = {blimp, NULL, NULL, NULL};

    // Start by matching the number of sub-trees against the lenghts of possible
    // reductions. This will narrow down the set of productions that could
    // potentially match this parse tree and eliminate the need to do bounds
    // checking everywhere.
    switch (Vector_Length(&tree->sub_trees)) {
        case 1:
            // Term -> "^msg"
            if (SubTreeIsTerminal(tree, 0, TOK_MSG_NAME)) {
                return MsgHandler(&ctx, tree);
            }
            // TermNoMsg -> "^"
            if (SubTreeIsTerminal(tree, 0, TOK_MSG_THIS)) {
                return MsgThisHandler(&ctx, tree);
            }
            // Trivial reductions do not change the parsed expression.
            return IdentityHandler(&ctx, tree);
        case 2:
            // Stmt[NoMsg] = Stmt[NoMsg] Term
            return SendHandler(&ctx, tree);
        case 3:
            // Expr[NoMsg] -> Stmt[NoMsg] Semi Expr
            if (SubTreeIsTerminal(tree, 1, TOK_SEMI)) {
                return SequenceHandler(&ctx, tree);
            }
            // TermNoMsg -> "(" Expr ")"
            if (SubTreeIsTerminal(tree, 0, TOK_LPAREN) &&
                SubTreeIsTerminal(tree, 2, TOK_RPAREN))
            {
                ctx.arg = (void *)1;
                return IdentityHandler(&ctx, tree);
            }
            // TermNoMsg -> "{" ExprNoMsg "}"
            if (SubTreeIsTerminal(tree, 0, TOK_LBRACE) &&
                SubTreeIsTerminal(tree, 2, TOK_RBRACE))
            {
                return BlockHandler(&ctx, tree);
            }
            break;
        case 4:
            // TermNoMsg -> "{" "^msg" Expr "}"
            if (SubTreeIsTerminal(tree, 0, TOK_LBRACE) &&
                SubTreeIsTerminal(tree, 1, TOK_MSG_NAME) &&
                SubTreeIsTerminal(tree, 3, TOK_RBRACE))
            {
                ctx.arg = (void *)1;
                return BlockHandler(&ctx, tree);
            }
            break;
    }

    return Error(blimp, BLIMP_INVALID_PARSE_TREE);
}
