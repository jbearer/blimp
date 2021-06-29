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
    ParseTree tree;
} ParseTreeArg;

static void ParseTreeFinalizer(void *p)
{
    ParseTreeArg *arg = (ParseTreeArg *)p;
    ParseTree_Destroy(&arg->tree);
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

    size_t num_trees = arg->tree.num_sub_trees;
    ParseTree *trees = arg->tree.sub_trees;

    // Send each sub-tree to the `message`.
    for (ParseTree *tree = trees; tree < trees + num_trees; ++tree) {
        // Get an object representing the sub-tree `tree`. This will be either a
        // symbol or a smaller parse tree extension object, depending on whether
        // `tree` is a terminal or a non-terminal with its own parse tree.
        Object *sub_tree;
        if (tree->symbol.is_terminal) {
            sub_tree = (Object *)tree->token;
        } else {
            // Allocate an argument for ParseTreeMethod() for the sub-tree.
            ParseTreeArg *sub_arg;
            TRY(Malloc(blimp, sizeof(ParseTreeArg), &sub_arg));
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
            if (ParseTree_Copy(blimp, tree, &sub_arg->tree) != BLIMP_OK) {
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
                ParseTree_Destroy(&sub_arg->tree);
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

static void ParseTreeVisitorFinalizer(void *p)
{
    Vector_Destroy((Vector *)p);
    free(p);
}

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

    ParseTree tree = {0};
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
        if (ParseTree_Copy(blimp, &arg->tree, tree) != BLIMP_OK) {
            goto error;
        }
        return BLIMP_OK;
    }

    if (Object_Type(obj) == OBJ_SYMBOL) {
        // Symbols represent terminals and vice versa.
        Token tok;
        if (GetToken(blimp, ((const Symbol *)obj)->name, &tok) != BLIMP_OK) {
            goto error;
        }

        tree->symbol = (GrammarSymbol){.is_terminal=true, .terminal=tok.type};
        tree->token = tok.symbol;
        return BLIMP_OK;
    }

    // Any non-symbol object represents a non-trivial parse tree, whose
    // sub-trees can be traversed by sending it a visitor. We will use a
    // ParseTreeVisitor to collect ParseTrees for each of the sub-trees in the
    // vector `trees`, and then we will parse that sequence of parse trees into
    // one larger tree.
    Vector/*<ParseTree>*/ *trees;
    TRY(Malloc(blimp, sizeof(Vector), &trees));
        // Allocate `trees` on the heap because the object we pass it to might
        // keep a reference to it that lives longer than this function.
    Vector_Init(
        blimp, trees, sizeof(ParseTree), (Destructor)ParseTree_Destroy);
    Object *visitor;
    if (BlimpObject_NewExtension(
            blimp,
            (Object *)blimp->global,
            trees,
            ParseTreeVisitor,
            ParseTreeVisitorFinalizer,
            &visitor
        ) != BLIMP_OK)
    {
        ParseTreeVisitorFinalizer(trees);
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
    Vector_MoveOut(trees, &tree->num_sub_trees, (void **)&tree->sub_trees);

    BlimpObject_Release(visitor);
        // Release `visitor` _after_ moving the `trees` vector to
        // `tree->sub_trees`, as the finalizer might destroy the `trees` vector.

    return BLIMP_OK;

error:
    // Make sure `tree` is a valid parse tree even if we failed.
    tree->symbol = (GrammarSymbol){.is_terminal=true, .terminal=TOK_INVALID};
    TRY(Blimp_GetSymbol(blimp, "", &tree->token));
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
        tree->token = tok.symbol;
        return BLIMP_OK;
    }

    // Any non-symbol object represents a non-trivial parse tree, whose
    // sub-trees can be traversed by sending it a visitor. We will use a
    // ParseTreeVisitor to collect ParseTrees for each of the sub-trees in the
    // vector `trees`, and then we will parse that sequence of parse trees into
    // one larger tree.
    Vector/*<ParseTree>*/ *trees;
    TRY(Malloc(blimp, sizeof(Vector), &trees));
        // Allocate `trees` on the heap because the object we pass it to might
        // keep a reference to it that lives longer than this function.
    Vector_Init(
        blimp, trees, sizeof(ParseTree), (Destructor)ParseTree_Destroy);
    Object *visitor;
    if (BlimpObject_NewExtension(
            blimp,
            (Object *)blimp->global,
            trees,
            ParseTreeVisitor,
            ParseTreeVisitorFinalizer,
            &visitor
        ) != BLIMP_OK)
    {
        ParseTreeVisitorFinalizer(trees);
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
    if (Reparse(trees, &blimp->grammar, nt, NULL, tree) != BLIMP_OK) {
        BlimpObject_Release(visitor);
        goto error;
    }
    BlimpObject_Release(visitor);
        // Release `visitor` _after_ the call to Reparse() above, as the
        // finalizer might destroy the `trees` vector.

    assert(!tree->symbol.is_terminal);
    assert(tree->symbol.non_terminal == nt);

    return BLIMP_OK;

error:
    // Make sure `tree` is a valid parse tree even if we failed.
    tree->symbol = (GrammarSymbol){.is_terminal=true, .terminal=TOK_INVALID};
    TRY(Blimp_GetSymbol(blimp, "", &tree->token));
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
    input_arg->non_terminal_symbol = arg->non_terminal_symbol;
    // We give the parse tree object a copy of the input trees, since, while
    // unlikely, the macro handler may save a reference to the parse tree which
    // lives longer than the input sub-trees.
    if (ParseTree_Copy(ctx->blimp, tree, &input_arg->tree) != BLIMP_OK) {
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
    //
    // This is a temporary hack. It would be better to simply keep the resulting
    // parse tree as-is, because
    //  * It makes the language simpler and easier to understand, and macros
    //    easier to work with, if the rule is: a macro simply replaces one parse
    //    tree with another, user-provided one, unmodified.
    //  * It makes it possible to work with macros which return bare symbols as
    //    their parse trees. Currently, this hack would change such a symbol
    //    from a terminal to a non-terminal with no sub-trees, making it
    //    impossible to recover the original symbol from bl:mp code using the
    //    parse tree API.
    // Unfortunately, reparsing requires us to update the parse tree here, so
    // that it can later be used where the macro non-terminal is expected when
    // it is reparsed as a sub-tree of another macro expansion. When we remove
    // reparsing, we should also fix this behavior.
    if (tree->symbol.is_terminal) {
        // Because of the hack described above, we are setting the symbol of the
        // parse tree to a specific non-terminal, and therefore the parse tree
        // must be a non-terminal. We cannot change a terminal parse tree to a
        // non-terminal, because the two kinds of parse trees have completely
        // different structures. This restriction will be removed when the rest
        // of this is removed, at which point macros will be able to return
        // terminals.
        return Blimp_ErrorFrom(ctx->blimp, *ctx->range, BLIMP_INVALID_PARSE_TREE,
            "macros returning symbols are not yet supported");
    }
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
    ParseTree *production_tree = SubTree(tree, 1);
    ParseTree *handler_tree    = SubTree(tree, 2);

    // Convert the production and handler parse trees to expressions so we can
    // evaluate them at parse time.
    Expr *production, *handler;
    TRY(BlimpParseTree_Eval(ctx->blimp, production_tree, &production));
    if (BlimpParseTree_Eval(ctx->blimp, handler_tree, &handler) != BLIMP_OK) {
        Blimp_FreeExpr(production);
        return Reraise(ctx->blimp);
    }

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
    return BLIMP_OK;
}

////////////////////////////////////////////////////////////////////////////////
// Default grammar
//

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
        NULL, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, ExprNoMsg, (NT(StmtNoMsg), NT(Semi), NT(Expr)),
        NULL, NULL));
    //      \ Stmt[NoMsg]
    TRY(ADD_GRAMMAR_RULE(grammar, Expr, (NT(Stmt)),
        NULL, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, ExprNoMsg, (NT(StmtNoMsg)),
        NULL, NULL));

    // Stmt[NoMsg] = Stmt[NoMsg] Term
    TRY(ADD_GRAMMAR_RULE(grammar, Stmt, (NT(Stmt), NT(Term)),
        NULL, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, StmtNoMsg, (NT(StmtNoMsg), NT(Term)),
        NULL, NULL));
    //      \> Term Term
    TRY(ADD_GRAMMAR_RULE(grammar, Stmt, (T(MACRO), NT(Term), NT(Term)),
        PrecedenceHandler, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, StmtNoMsg, (T(MACRO), NT(Term), NT(Term)),
        PrecedenceHandler, NULL));
    //      \ term
    TRY(ADD_GRAMMAR_RULE(grammar, Stmt, (NT(Term)),
        NULL, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, StmtNoMsg, (NT(TermNoMsg)),
        NULL, NULL));

    // Semi = ";"
    TRY(ADD_GRAMMAR_RULE(grammar, Semi, (T(SEMI)),
        NULL, NULL));

    // Term = TermNoMsg \ Msg
    TRY(ADD_GRAMMAR_RULE(grammar, Term, (NT(TermNoMsg)),
        NULL, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, Term, (T(MSG_NAME)),
        NULL, NULL));

    // TermNoMsg = "(" expr ")"
    TRY(ADD_GRAMMAR_RULE(grammar, TermNoMsg, (T(LPAREN), NT(Expr), T(RPAREN)),
        NULL, NULL));
    //      \ "{" [msg] expr "}"
    TRY(ADD_GRAMMAR_RULE(grammar, TermNoMsg, (T(LBRACE), T(MSG_NAME), NT(Expr), T(RBRACE)),
        NULL, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, TermNoMsg, (T(LBRACE), NT(ExprNoMsg), T(RBRACE)),
        NULL, NULL));
    //      \ symbol
    TRY(ADD_GRAMMAR_RULE(grammar, TermNoMsg, (T(SYMBOL)),
        NULL, NULL));
    //      \ "^"
    TRY(ADD_GRAMMAR_RULE(grammar, TermNoMsg, (T(MSG_THIS)),
        NULL, NULL));

    // Give the parser readable names for the built-in terminals (user-defined
    // terminals will be named by the string used to create the terminal; see
    // CreateTerminal()).
    for (TokenType t = 0; t < NUM_BUILT_IN_TOKENS; ++t) {
        Grammar_SetTerminalString(grammar, t, StringOfTokenType(t));
    }

    return BLIMP_OK;
}

////////////////////////////////////////////////////////////////////////////////
// Converting ParseTrees to Exprs
//
// In bl:mp semantics, this step is a no-op because parse trees in the basic
// grammar, after macro expansions, are already expressions. That is, a parse
// tree made up of only primitive productions is the same S-expression as one of
// the bl:mp expression forms.
//
// In our implementation of the semantics, though, ParseTree data structures
// represent S-expressions while the Expr type is a more structured
// representation with extra metadata and less redundant information (for
// example, we don't keep track of extra, semantic-less lexical artifacts like
// parentheses in Exprs). So we need to match the form of the parse tree against
// one of the primitive productions and construct the appropriate Expr.
//
// Converting a ParseTree to an Expr requires a bottom-up traversal of the parse
// tree, where sub-trees are converted to sub-expressions of their parent tree.
// Such a bottom-up traversal is easy to write recursively, but parse trees can
// get quite large (and deep, since sequences of expressions are reprsented as
// nested parse trees) and we might not have the stack space to do this
// traversal recursively. So, we implement an iterative bottom-up (post-order
// traversal).
//
// Post-order is probably the trickiest tree traversal to do iteratively.
// Luckily though, all of the primitive expression forms have at most two
// sub-trees which need to be converted to expressions in order to construct the
// parent expression, so we can treat a parse tree as a binary tree rather than
// an N-ary tree. There is a relatively straight-forward solution for iterative
// post-order traversal of binary trees.
//
// The ParseTreeNode type maps a parse tree to a node in a binary tree. It also
// includes some extra metadata which allows us to map each parse tree to an
// expression form and a binary tree node at most once. Once we have a
// ParseTreeNode for a ParseTree, we don't have to inspect the sub-trees of a
// parse tree to find out what kind of expression it is (for example, is the
// first sub-tree a `(` terminal?) because this information is encoded in the
// `type` field of the ParseTreeNode. And we don't have to figure out what kind
// of expression it is to determine whether it has 0, 1, or 2 sub-expressions,
// or which sub-trees correspond to the sub-expressions, because the relevant
// sub-trees are already stored in the `left` and `right` fields.
//
// By convention, if a parse tree has exactly 1 sub-tree, it is `left`, and
// `right` is NULL.
typedef struct {
    enum {
        PARSE_TREE_TERMINAL,
        PARSE_TREE_TRIVIAL,
        PARSE_TREE_MSG,
        PARSE_TREE_MSG_THIS,
        PARSE_TREE_SEND,
        PARSE_TREE_SEQUENCE,
        PARSE_TREE_PARENS,
        PARSE_TREE_BLOCK_NO_MSG,
        PARSE_TREE_BLOCK,
        PARSE_TREE_MACRO,
    } type;

    ParseTree *tree;
    ParseTree *left;
    ParseTree *right;
} ParseTreeNode;

static inline bool SubTreeIsTerminal(
    const ParseTree *tree, size_t index, Terminal t)
{
    const ParseTree *sub_tree = SubTree(tree, index);
    if (sub_tree->symbol.is_terminal) {
        return sub_tree->symbol.terminal == t;
    } else if (sub_tree->num_sub_trees == 1) {
        return SubTreeIsTerminal(sub_tree, 0, t);
    } else {
        return false;
    }
}

static inline const Symbol *SubToken(
    const ParseTree *tree, size_t index)
{
    const ParseTree *sub_tree = SubTree(tree, index);
    if (sub_tree->symbol.is_terminal) {
        return sub_tree->token;
    } else {
        assert(sub_tree->num_sub_trees == 1);
        return SubToken(sub_tree, 0);
    }
}

// ParseTreeToNode() correlates a ParseTree with one of the primitive expression
// forms and stores this information in a ParseTreeNode data structure, so that
// later on we do not have to inspect the ParseTree to find relevant information
// such as which of its sub-trees need to be converted to expressions or what
// kind of expression it represents.
//
// If the ParseTree does not correspond to a built-in expression form (for
// example, a user-defined macro returned an ad-hoc parse tree structure which
// erroneously made it into the final expression tree) this is where we will
// catch and report those errors.
//
// Right now, this logic is a bit complicated, as it is basically an ad-hoc
// parser for the built-in bl:mp grammar. However, the long-term goal is to
// simplify the built-in grammar so that it contains only one production for
// each primitive syntactic construct (symbol and block literals, macros, and
// sends) and so that each production starts with a prefix terminal which
// indicates the production being used. Then, this logic will simply amount to
// checking the prefix terminal, validating the rest of the tree, and calling
// the appropriate handler.
static Status ParseTreeToNode(
    Blimp *blimp, ParseTree *tree, ParseTreeNode *node)
{
    node->tree = tree;

    // By default, set `left` and `right` to NULL. If either sub-expression is
    // present, we will set it when we determine which expression form
    // corresponds to the parse tree.
    node->left = NULL;
    node->right = NULL;

    if (tree->symbol.is_terminal) {
        // Terminals have no sub-trees.
        node->type = PARSE_TREE_TERMINAL;
        return BLIMP_OK;
    }

    // Get the terminals we will need to check for to interpret the built-in
    // expression forms.
    Terminal TOK_SEMI, TOK_LBRACE, TOK_RBRACE, TOK_LPAREN, TOK_RPAREN, TOK_MACRO;
    TRY(CreateTerminal(blimp, ";", &TOK_SEMI));
    TRY(CreateTerminal(blimp, "{", &TOK_LBRACE));
    TRY(CreateTerminal(blimp, "}", &TOK_RBRACE));
    TRY(CreateTerminal(blimp, "(", &TOK_LPAREN));
    TRY(CreateTerminal(blimp, ")", &TOK_RPAREN));
    TRY(CreateTerminal(blimp, "\\>", &TOK_MACRO));

    // Start by matching the number of sub-trees against the lengths of possible
    // reductions. This will narrow down the set of productions that could
    // potentially match this parse tree and eliminate the need to do bounds
    // checking everywhere.
    switch (tree->num_sub_trees) {
        case 1:
            // Term -> "^msg"
            if (SubTreeIsTerminal(tree, 0, TOK_MSG_NAME)) {
                node->type = PARSE_TREE_MSG;
                return BLIMP_OK;
            }
            // TermNoMsg -> "^"
            if (SubTreeIsTerminal(tree, 0, TOK_MSG_THIS)) {
                node->type = PARSE_TREE_MSG_THIS;
                return BLIMP_OK;
            }
            // Trivial reductions do not change the parsed expression, we will
            // just use the expression resulting from the sub-tree.
            node->type = PARSE_TREE_TRIVIAL;
            node->left = SubTree(tree, 0);
            return BLIMP_OK;
        case 2: {
            // Stmt[NoMsg] = Stmt[NoMsg] Term
            node->type = PARSE_TREE_SEND;
            node->left = SubTree(tree, 0);
            node->right = SubTree(tree, 1);
            return BLIMP_OK;
        }
        case 3:
            // Expr[NoMsg] -> Stmt[NoMsg] Semi Expr
            if (SubTreeIsTerminal(tree, 1, TOK_SEMI)) {
                node->type = PARSE_TREE_SEQUENCE;
                node->left = SubTree(tree, 0);
                node->right = SubTree(tree, 2);
                return BLIMP_OK;
            }
            // TermNoMsg -> "(" Expr ")"
            if (SubTreeIsTerminal(tree, 0, TOK_LPAREN) &&
                SubTreeIsTerminal(tree, 2, TOK_RPAREN))
            {
                node->type = PARSE_TREE_PARENS;
                node->left = SubTree(tree, 1);
                return BLIMP_OK;
            }
            // TermNoMsg -> "{" ExprNoMsg "}"
            if (SubTreeIsTerminal(tree, 0, TOK_LBRACE) &&
                SubTreeIsTerminal(tree, 2, TOK_RBRACE))
            {
                node->type = PARSE_TREE_BLOCK_NO_MSG;
                node->left = SubTree(tree, 1);
                return BLIMP_OK;
            }
            // Stmt[NoMsg] -> "\>"" Term Term
            if (SubTreeIsTerminal(tree, 0, TOK_MACRO)) {
                node->type = PARSE_TREE_MACRO;

                // Currently, the expression resulting from a macro definition
                // is a dummy symbol, to have no side-effects. In the future,
                // though, we will have a built-in macro definition expression
                // type, which defines a macro at runtime when evaluated. That
                // will require converting the sub-Terms to expressions, so we
                // include the left and right sub-trees here to ensure current
                // bl:mp code is forwards compatible; that is, both sub-trees of
                // a macro definition must be valid parse trees. For now,
                // ParseTreeNodeToExpr will simply discard the resulting left
                // and right sub-expressions after the parse trees have been
                // checked for validity.
                node->left = SubTree(tree, 1);
                node->right = SubTree(tree, 2);
                return BLIMP_OK;
            }
            break;
        case 4:
            // TermNoMsg -> "{" "^msg" Expr "}"
            if (SubTreeIsTerminal(tree, 0, TOK_LBRACE) &&
                SubTreeIsTerminal(tree, 1, TOK_MSG_NAME) &&
                SubTreeIsTerminal(tree, 3, TOK_RBRACE))
            {
                node->type = PARSE_TREE_BLOCK;
                node->left = SubTree(tree, 2);
                return BLIMP_OK;
            }
            break;
    }

    return Error(blimp, BLIMP_INVALID_PARSE_TREE);
}

static Status ParseTreeNodeToExpr(
    Blimp *blimp, const ParseTreeNode *node, Expr *left, Expr *right, Expr **expr)
{
    const ParseTree *tree = node->tree;

    switch (node->type) {
        case PARSE_TREE_TERMINAL:
            return BlimpExpr_NewSymbol(blimp, tree->token, expr);
        case PARSE_TREE_TRIVIAL:
            *expr = left;
            return BLIMP_OK;
        case PARSE_TREE_MSG: {
            // Term -> "^msg"
            const Symbol *tok = SubToken(tree, 0);
            assert(tok->name[0] == '^');
            assert(tok->name[1] != '\0');

            const Symbol *sym;
            TRY(Blimp_GetSymbol(blimp, &tok->name[1], &sym));
            return BlimpExpr_NewMsgName(blimp, sym, expr);
        }
        case PARSE_TREE_MSG_THIS:
            // TermNoMsg -> "^"
            return BlimpExpr_NewMsgIndex(blimp, 0, expr);
        case PARSE_TREE_SEND:
            // Stmt[NoMsg] = Stmt[NoMsg] Term
            return BlimpExpr_NewSend(blimp, left, right, expr);
        case PARSE_TREE_SEQUENCE:
            // Expr[NoMsg] -> Stmt[NoMsg] Semi Expr
            //
            // The sub-statement may already be a sequence, so combining it
            // with the sub-expression amounts to appending two linked
            // lists.
            left->last->next = right;
            left->last = right->last;
            *expr = left;
            return BLIMP_OK;
        case PARSE_TREE_PARENS:
            // TermNoMsg -> "(" Expr ")"
            *expr = left;
            return BLIMP_OK;
        case PARSE_TREE_BLOCK_NO_MSG:
            // TermNoMsg -> "{" ExprNoMsg "}"
            return BlimpExpr_NewBlock(blimp, NULL, left, expr);
       case PARSE_TREE_BLOCK: {
            // TermNoMsg -> "{" "^msg" Expr "}"
            const Symbol *msg_name;
            const Symbol *msg_tok = SubToken(tree, 1);
            assert(msg_tok->name[0] == '^');
            assert(msg_tok->name[1] != '\0');
            TRY(Blimp_GetSymbol(blimp, &msg_tok->name[1], &msg_name));

            return BlimpExpr_NewBlock(blimp, msg_name, left, expr);
        }
        case PARSE_TREE_MACRO: {
            // Stmt[NoMsg] -> "\>"" Term Term
            //
            // Currently, we simply return a dummy expression which has no
            // runtime side-effects (a `.` symbol), and thus we discard the two
            // sub-expressions, which we only wanted to check for validity.
            //
            // In the future, we will use these sub-expressions to construct a
            // built-in "macro definition" expression, which defines a macro at
            // runtime when evaluated.
            Blimp_FreeExpr(left);
            Blimp_FreeExpr(right);

            const Symbol *dot;
            TRY(Blimp_GetSymbol(blimp, ".", &dot));
            return BlimpExpr_NewSymbol(blimp, dot, expr);
        }

        default:
            abort();
            return BLIMP_OK;
    }
}

Status ParseTreeToExpr(Blimp *blimp, ParseTree *tree, Expr **result)
{
    // Iterative post-order traversal. We will use two stacks. The idea is to
    // get the nodes in reverse post-order onto the `reverse_post_order` stack.
    // This is based on the observation that the reverse of post-order is
    // pre-order with left and right reversed, and pre-order is easy to
    // implement iteratively using one stack (it is just a depth-first
    // traversal).
    //
    // We use `stack` to implement a pre-order traversal (but always visiting
    // `right` first instead of `left` first) and each time the pre-order
    // traversal would visit a node, we push it onto `reverse_post_order`. At
    // the end of this process, `reverse_post_order` will contain all the
    // required sub-trees in the reverse of the order we want to visit them.
    //
    // Also, we will convert ParseTrees to ParseTreeNodes as we push them onto
    // `stack`, and our traversal will be over the binary tree structure imposed
    // by the ParseTreeNode type.
    Vector/*<ParseTreeNode>*/ stack;
    Vector/*<ParseTreeNode>*/ reverse_post_order;

    Vector_Init(blimp, &stack, sizeof(ParseTreeNode), NULL);
    Vector_Init(blimp, &reverse_post_order, sizeof(ParseTreeNode), NULL);

    // The first node visited in a pre-order traversal is the root, so convert
    // it to a ParseTreeNode and push it onto the stack.
    ParseTreeNode *root;
    TRY(Vector_EmplaceBack(&stack, (void **)&root));
    if (ParseTreeToNode(blimp, tree, root) != BLIMP_OK) {
        Vector_Destroy(&stack);
        Vector_Destroy(&reverse_post_order);
        return Reraise(blimp);
    }

    // Depth-first traversal.
    while (!Vector_Empty(&stack)) {
        // Remove the first node from the stack and "visit" it by adding it to
        // `reverse_post_order`.
        ParseTreeNode node;
        Vector_PopBack(&stack, &node);
        if (Vector_PushBack(&reverse_post_order, &node) != BLIMP_OK) {
            Vector_Destroy(&stack);
            Vector_Destroy(&reverse_post_order);
            return Reraise(blimp);
        }

        // Push the node's children (if any) onto the stack, converting them to
        // ParseTreeNodes in the process.
        if (node.left != NULL) {
            ParseTreeNode *left;
            if (Vector_EmplaceBack(&stack, (void **)&left) != BLIMP_OK ||
                ParseTreeToNode(blimp, node.left, left) != BLIMP_OK)
            {
                Vector_Destroy(&stack);
                Vector_Destroy(&reverse_post_order);
                return Reraise(blimp);
            }
        }
        if (node.right != NULL) {
            ParseTreeNode *right;
            if (Vector_EmplaceBack(&stack, (void **)&right) != BLIMP_OK ||
                ParseTreeToNode(blimp, node.right, right) != BLIMP_OK)
            {
                Vector_Destroy(&stack);
                Vector_Destroy(&reverse_post_order);
                return Reraise(blimp);
            }
        }
    }
    Vector_Destroy(&stack);

    // Now `reverse_post_order` is a stack containing the nodes in post order.
    // Pop from it until it is empty (popping in LIFO order reverse the nodes so
    // that we see them in the correct order) and convert each node to an
    // expression as we go.
    //
    // We keep a stack of the expressions we have converted. The post-order,
    // bottom-up traversal ensures that by the time we reach a new node, the
    // sub-expressions it requires to construct its own expression are the top
    // 0, 1, or 2 expressions on the `sub_exprs` stack, as appropriate. We can
    // thus pop the appropriate number of expressions from `sub_exprs` and pass
    // them to ParseTreeNodeToExpr() along with the node.
    Vector/*<Expr *>*/ sub_exprs;
    Vector_Init(blimp, &sub_exprs, sizeof(Expr *), ExprDestructor);
    while (!Vector_Empty(&reverse_post_order)) {
        ParseTreeNode node;
        Vector_PopBack(&reverse_post_order, &node);

        // Get sub-expressions from the `sub_exprs` stack as needed.
        Expr *left = NULL;
        Expr *right = NULL;
        if (node.right != NULL) {
            Vector_PopBack(&sub_exprs, &right);
        }
        if (node.left != NULL) {
            Vector_PopBack(&sub_exprs, &left);
        }

        // Convert to an expression.
        Expr *expr;
        if (ParseTreeNodeToExpr(
                blimp, &node, left, right, &expr) != BLIMP_OK)
        {
            if (left != NULL) {
                Blimp_FreeExpr(left);
            }
            if (right != NULL) {
                Blimp_FreeExpr(right);
            }
            Vector_Destroy(&sub_exprs);
            Vector_Destroy(&reverse_post_order);
            return Reraise(blimp);
        }

        // Copy the source range from the parse tree to the expression.
        if (!BlimpExpr_HasSourceRange(expr)) {
            BlimpExpr_SetSourceRange(expr, &node.tree->range);
        }

        // Add the new expression to the `sub_exprs` stack.
        if (Vector_PushBack(&sub_exprs, &expr) != BLIMP_OK) {
            Blimp_FreeExpr(expr);
            Vector_Destroy(&sub_exprs);
            Vector_Destroy(&reverse_post_order);
            return Reraise(blimp);
        }
    }
    Vector_Destroy(&reverse_post_order);

    // At the end, the only expression remaining on the `sub_exprs` stack is the
    // expression corresponding to the root of the tree.
    assert(Vector_Length(&sub_exprs) == 1);
    Vector_PopBack(&sub_exprs, result);
    Vector_Destroy(&sub_exprs);

    return BLIMP_OK;
}
