#include "internal/blimp.h"
#include "internal/expr.h"
#include "internal/grammar.h"

////////////////////////////////////////////////////////////////////////////////
// Lexer API
//

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

static void ParseTreeFinalizer(void *arg)
{
    ParseTree *tree = (ParseTree *)arg;
    ParseTree_Release(tree);
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

    ParseTree *tree;
    CHECK(BlimpObject_ParseExtension(receiver, NULL, (void **)&tree));

    size_t num_trees = tree->num_sub_trees;
    ParseTree **sub_trees = tree->sub_trees;

    // Send each sub-tree to the `message`.
    for (ParseTree **sub_tree = sub_trees;
         sub_tree < sub_trees + num_trees;
         ++sub_tree)
    {
        // Get an object representing the sub-tree `sub_tree`. The sub-tree must
        // own a reference to `sub_tree` since, while unlikely, the visitor to
        // which we are going to send the sub-tree may save a reference to it
        // which lives longer than the ParseTree `tree`.
        Object *sub_tree_obj;
        TRY(BlimpObject_NewExtension(
            blimp,
            (Object *)blimp->global,
            ParseTree_Borrow(*sub_tree),
            ParseTreeMethod,
            ParseTreeFinalizer,
            &sub_tree_obj
        ));

        // Send the sub-tree to the `message`.
        if (Blimp_Send(
                blimp, (Object *)blimp->global, message, sub_tree_obj, NULL)
            != BLIMP_OK)
        {
            BlimpObject_Release(sub_tree_obj);
            return Reraise(blimp);
        }
        BlimpObject_Release(sub_tree_obj);
    }

    *result = BlimpObject_Borrow(tree->symbol);
    return BLIMP_OK;
}

typedef struct {
    Vector/*<ParseTree *>*/ trees;
} ParseTreeVisitorArg;

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

    if (blimp->options.loop_errors) {
        // A somewhat common error is to accidentally pass a parse tree visitor
        // to another parse tree visitor. (Missing semi-colons is one very easy
        // way for this to happen.) This construct will always cause an infinite
        // loop, since the receiving visitor attempts to convert the message
        // visitor to a parse tree by creating a new visitor and sending it to
        // the message visitor, and this process repeats ad nauseum. Rather than
        // opaquely loop forever when this happens (or worse, overflow the stack
        // and segfault) we can be a little friendlier and report an error.
        BlimpMethod method;
        if (BlimpObject_ParseExtension(message, &method, NULL) == BLIMP_OK &&
            method == ParseTreeVisitor)
        {
            return ErrorMsg(blimp, BLIMP_INFINITE_LOOP,
                "loop detected: macro created infinite parse tree "
                "(did you accidentally return a parse tree visitor?)");
        }
    }

    ParseTreeVisitorArg *arg;
    CHECK(BlimpObject_ParseExtension(receiver, NULL, (void **)&arg));

    // Because this object gets passed to an arbitrary object, it may live for a
    // long time, even though its purpose (appending parse trees to
    // `arg->trees`) is fulfilled in the space of a single interpreter function
    // call. Since the side-effects of this function are not observable outside
    // the single interpreter function in which it is created, we can avoid a
    // needlessly long-lived allocation by only performing the effects if this
    // method is invoked in a context where they are still meaningful. The
    // caller will nullify `arg` once the interpreter moves on from the context
    // in which the effects of this method have meaning.
    if (arg != NULL) {
        ParseTree *tree;
        TRY(BlimpObject_ToParseTree(message, &tree));
            // Recursively visit the sub-trees of the parse tree object we are
            // visiting and convert those sub-trees into a single ParseTree.
            // Essentially, this converts the Object which represents a parse
            // tree into an actual ParseTree data structure.
        TRY(Vector_PushBack(&arg->trees, &tree));
    }

    *result = BlimpObject_Borrow(receiver);
        // Return the receiver so the user can chain message sends to easily
        // send a sequence of sub-trees to the receiver.
    return BLIMP_OK;
}

// Extract a ParseTree from an Object which conforms to the parse tree protocol.
Status BlimpObject_ToParseTree(Object *obj, ParseTree **tree)
{
    Blimp *blimp = Object_Blimp(obj);

    ParseTree *tree_arg;
    BlimpMethod method;
    if (BlimpObject_ParseExtension(obj, &method, (void **)&tree_arg) == BLIMP_OK
        && method == ParseTreeMethod)
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
        *tree = ParseTree_Borrow(tree_arg);
        return BLIMP_OK;
    }

    // Use a ParseTreeVisitor to collect ParseTrees for each of the sub-trees of
    // this object in the vector `arg.trees`. We will collect that sequence of
    // parse trees into one larger tree.
    ParseTreeVisitorArg arg;
    Vector_Init(
        blimp, &arg.trees, sizeof(ParseTree *), ParseTreeDestructor);
    Object *visitor;
    if (BlimpObject_NewExtension(
            blimp,
            (Object *)blimp->global,
            &arg,
            ParseTreeVisitor,
            NULL,
            &visitor
        ) != BLIMP_OK)
    {
        Vector_Destroy(&arg.trees);
        goto error;
    }

    // Send the visitor to the object to collect its sub-trees, and get the
    // grammar symbol of the object, which is the return value of the parse tree
    // object.
    Object *symbol;
    Status ret = Blimp_Send(
        blimp, (Object *)blimp->global, obj, visitor, &symbol);
    // We no longer need the visitor object. However, it will not necessarily be
    // destroyed when we release it, because `obj` may have stored a reference
    // to it. Therefore, to ensure that it doesn't try to access the stack-
    // allocated `arg` after we return from this function, we need to NULL out
    // its state before releasing it.
    CHECK(BlimpObject_SetExtensionState(visitor, NULL));
    BlimpObject_Release(visitor);
    if (ret != BLIMP_OK) {
        Vector_Destroy(&arg.trees);
        goto error;
    }

    // Collect the parsed sub-trees into a new parse tree.
    ParseTree **sub_trees;
    size_t num_sub_trees;
    Vector_MoveOut(&arg.trees, &num_sub_trees, (void **)&sub_trees);
    Vector_Destroy(&arg.trees);
    ret = ParseTree_New(
        blimp, symbol, sub_trees, num_sub_trees, NULL, tree);
    BlimpObject_Release(symbol);
    return ret;

error:
    // Make sure `tree` is a valid parse tree even if we failed.
    TRY(Blimp_GetSymbol(blimp, "", (const Symbol **)&symbol));
    TRY(ParseTree_New(blimp, symbol, NULL, 0, NULL, tree));
    return Reraise(blimp);
}

typedef struct {
    Object *handler;
    const Symbol *non_terminal_symbol;
    SourceRange *macro_def_range;
} MacroArg;

// Handler called when a macro is reduced.
static Status MacroHandler(ParserContext *ctx, ParseTree **tree)
{
    MacroArg *arg = (MacroArg *)ctx->arg;
    Object *handler = arg->handler;

    // Create the argument for a parse tree extension object which we will use
    // to convey the input sub-trees to the handler object. The parse tree
    // object must own a reference to the input tree, since, while unlikely, the
    // macro handler may save a reference to the parse tree which lives longer
    // than the input sub-trees.
    Object *input_tree;
    TRY(BlimpObject_NewExtension(
        ctx->blimp,
        (Object *)ctx->blimp->global,
        ParseTree_Borrow(*tree),
        ParseTreeMethod,
        ParseTreeFinalizer,
        &input_tree
    ));

    // Send the input tree to the macro handler, resulting in an Object
    // representing the output parse tree. This result object (`output_tree`)
    // must conform to the parse tree protocol, so we can recover a ParseTree
    // from it using BlimpObject_ToParseTree().
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
        return ReraiseFromOpt(ctx->blimp, ctx->range);
    }
    BlimpObject_Release(input_tree);

    // Interpret the output Object.
    ParseTree *output;
    if (BlimpObject_ToParseTree(output_tree, &output) != BLIMP_OK) {
        BlimpObject_Release(output_tree);
        return ReraiseFromOpt(ctx->blimp, ctx->range);
    }
    BlimpObject_Release(output_tree);

    // Save the source range from where the macro was invoked so we can restore
    // it after overwriting the parse tree. We have to do this now because
    // `ctx->range` may be deallocated during ParseTree_Release().
    //
    // Note that we can improve error messages by _additionally_ tagging the
    // output tree with the location of the macro _definition_, or with a full
    // macro backtrace, but parse trees do not yet support multiple source
    // ranges.
    SourceRange range = (*tree)->range;

    // Replace the input tree with the output one.
    ParseTree_Release(*tree);
    *tree = output;

    // Restore the source range to the location where the macro was invoked.
    (*tree)->range = range;

    return BLIMP_OK;
}

Status DefineMacro(
    Blimp *blimp,
    Object *production,
    Object *handler,
    const SourceRange *range,
    const Symbol **nt_sym)
{
    // Convert the production into a ParseTree in order to extract a
    // non-terminal symbol and a list of grammar symbols for the new production.
    ParseTree *tree;
    TRY_FROM(range, BlimpObject_ToParseTree(production, &tree));
    if (tree->grammar_symbol.is_terminal) {
        // The production of a macro definition must be a non-terminal parse
        // tree, because we cannot define new rules for how terminals are
        // parsed.
        //
        // In the future, it may be useful to lift this restriction by creating
        // an injection from terminals to generated non-terminals, so that we
        // can define terminals dynamically. The capability of using a
        // non-trivial parse tree as a terminal will be useful, for example, for
        // handling object literals in macro definition productions, since
        // object literals are terminals generated by a non-trivial parse trees
        // of the form (! <expr>).
        ParseTree_Release(tree);
        return ErrorFromOpt(blimp, range, BLIMP_INVALID_PARSE_TREE,
            "the production of a macro definition must be a non-temrinal");
    }
    assert(Object_Type(tree->symbol) == OBJ_SYMBOL);
    *nt_sym = (const Symbol *)tree->symbol;

    // Extract the list of grammar symbols for the new production.
    Vector symbols;
    Vector_Init(blimp, &symbols, sizeof(GrammarSymbol), NULL);
    for (size_t i = 0; i < tree->num_sub_trees; ++i) {
        ParseTree *sub_tree = tree->sub_trees[i];
        GrammarSymbol *symbol;
        if (Vector_EmplaceBack(&symbols, (void **)&symbol) != BLIMP_OK) {
            Vector_Destroy(&symbols);
            ParseTree_Release(tree);
            return ReraiseFromOpt(blimp, range);
        }
        *symbol = sub_tree->grammar_symbol;
        if (symbol->is_terminal) {
            // For the time being, we require the terminal to be a Symbol.
            // Eventually, we may be able to remove this somewhat arbitrary
            // restriction and allow terminals in macro definitions to be
            // arbitrary Objects, though some design work is needed to decide
            // exactly what this should mean.
            const Symbol *term_sym;
            if (BlimpObject_ParseSymbol(sub_tree->symbol, &term_sym)
                    != BLIMP_OK)
            {
                Vector_Destroy(&symbols);
                ParseTree_Release(tree);
                return ReraiseFromOpt(blimp, range);
            }

            // If the grammar symbol is a terminal, it may represent a new
            // terminal which has not yet been added to the lexer. Add it now.
            if (CreateTerminal(blimp, term_sym->name, &symbol->terminal)
                    != BLIMP_OK)
            {
                Vector_Destroy(&symbols);
                ParseTree_Release(tree);
                return ReraiseFromOpt(blimp, range);
            }
        }
    }
    ParseTree_Release(tree);

    // Collect information needed by MacroHandler().
    MacroArg *handler_arg;
    if (Malloc(blimp, sizeof(MacroArg), &handler_arg) != BLIMP_OK) {
        Vector_Destroy(&symbols);
        return ReraiseFromOpt(blimp, range);
    }
    handler_arg->handler = BlimpObject_Borrow(handler);
    handler_arg->non_terminal_symbol = *nt_sym;
    if (range != NULL) {
        if (Malloc(blimp, sizeof(SourceRange), &handler_arg->macro_def_range)
                != BLIMP_OK)
        {
            Vector_Destroy(&symbols);
            return ReraiseFromOpt(blimp, range);
        }
        *handler_arg->macro_def_range = *range;
    }

    // Add a new production which will be handled by `handler`.
    NonTerminal nt;
    TRY(Grammar_GetNonTerminal(&blimp->grammar, *nt_sym, &nt));
    if (Grammar_AddRule(
            &blimp->grammar,
            nt,
            Vector_Length(&symbols),
            Vector_Data(&symbols),
            MacroHandler,
            handler_arg)
        != BLIMP_OK)
    {
        Vector_Destroy(&symbols);
        BlimpObject_Release(handler);
        free(handler_arg);
        return ReraiseFromOpt(blimp, range);
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
    NonTerminal NT_Expr, NT_ExprNoMsg, NT_Stmt, NT_StmtNoMsg, NT_Custom,
        NT_CustomNoMsg, NT_Semi, NT_Term, NT_TermNoMsg, NT_Run;
    TRY(GetNonTerminal(grammar,"_1", &NT_Expr));
    TRY(GetNonTerminal(grammar,"_2", &NT_ExprNoMsg));
    TRY(GetNonTerminal(grammar,"_3", &NT_Stmt));
    TRY(GetNonTerminal(grammar,"_4", &NT_StmtNoMsg));
    TRY(GetNonTerminal(grammar,"_5", &NT_Semi));
    // The bootstrap prelude needs some grammar symbol to prepend to the file it
    // is bootstrapping to force that file to execute. It uses the symbol __run,
    // but this cannot simply be a terminal, since leaving an unreduced terminal
    // on the stack throughout the parsing of the bootstrapee would make it
    // impossible for the bootstrapee to add and use new grammar rules. The
    // __run terminal must be immediately reduced to a non-terminal with lower
    // precedence than any rule the user might want to add. Since the user
    // defines new rules at built-in precedence levels (custom1 and higher) any
    // non-terminal defined in bootstrap prelude will have too high precedence.
    // Therefore, we build in a non-terminal __run for this specific purpose,
    // with precedence just lower than the first user-editable non-terminal.
    //
    // This is ugly, but it arises from the fact that the user-facing grammar
    // defined by bootstrap is totally entangled with the built-in grammar.
    // Eventually, the built-in grammar should be stripped down to a single,
    // very low-precedence non-terminal, and bootstrap should be able to define
    // __run itself.
    TRY(GetNonTerminal(grammar,"___run", &NT_Run));
    // It's useful to have precedences in between the statement grammar (3, 4)
    // and the term grammar (6, 7). The user cannot define new left-recursive
    // forms at precedence 5 or lower, because there will always be statements
    // (3) and semi-colons (5) on the parser stack, and so new forms with a
    // lower precedence initial symbol will not be added to the parser state. On
    // the other hand, terms cannot be left-recursive at all, since they are the
    // highest precedence level, and that would create ambiguity.
    TRY(GetNonTerminal(grammar,"_custom1", &NT_Custom));
    TRY(GetNonTerminal(grammar,"_custom2", &NT_CustomNoMsg));
    TRY(GetNonTerminal(grammar,"_6", &NT_Term));
    TRY(GetNonTerminal(grammar,"_7", &NT_TermNoMsg));

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

    // Stmt[NoMsg] = Stmt[NoMsg] Custom
    TRY(ADD_GRAMMAR_RULE(grammar, Stmt, (NT(Stmt), NT(Custom)),
        NULL, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, StmtNoMsg, (NT(StmtNoMsg), NT(Custom)),
        NULL, NULL));
    //      \> Stmt Custom
    TRY(ADD_GRAMMAR_RULE(grammar, Stmt, (T(MACRO), NT(Stmt), NT(Custom)),
        NULL, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, StmtNoMsg, (T(MACRO), NT(Stmt), NT(Custom)),
        NULL, NULL));
    //      \ Custom[NoMsg]
    TRY(ADD_GRAMMAR_RULE(grammar, Stmt, (NT(Custom)),
        NULL, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, StmtNoMsg, (NT(CustomNoMsg)),
        NULL, NULL));

    // Custom[NoMsg] = Term[NoMsg]
    TRY(ADD_GRAMMAR_RULE(grammar, Custom, (NT(Term)), NULL, NULL));
    TRY(ADD_GRAMMAR_RULE(grammar, CustomNoMsg, (NT(TermNoMsg)), NULL, NULL));

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
    //      \ "!" expr
    TRY(ADD_GRAMMAR_RULE(grammar, TermNoMsg, (T(BANG), NT(Term)),
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

// Extract a ParseTree from an Object which conforms to the parse tree protocol.
// This function modifies the parse tree represented by `obj` by running its
// sequence of sub-trees through the parser, with its grammar symbol as a goal.
static Status ParseObject(Blimp *blimp, Object *obj, ParseTree **tree)
{
    // Use a ParseTreeVisitor to collect ParseTrees for each of the sub-trees
    // into the vector `arg.trees`. We will then parse that sequence of parse
    // trees into one larger tree.
    ParseTreeVisitorArg arg;
    Vector_Init(blimp, &arg.trees, sizeof(ParseTree *), ParseTreeDestructor);
    Object *visitor;
    if (BlimpObject_NewExtension(
            blimp,
            (Object *)blimp->global,
            &arg,
            ParseTreeVisitor,
            NULL,
            &visitor
        ) != BLIMP_OK)
    {
        goto error;
    }

    // Send the visitor to the object to collect its sub-trees, and get the
    // grammar symbol of the parse tree, which is the return value of the parse
    // tree object.
    Object *symbol;
    Status ret = Blimp_Send(
        blimp, (Object *)blimp->global, obj, visitor, &symbol);
    // We no longer need the visitor object. However, it will not necessarily be
    // destroyed when we release it, because `obj` may have stored a reference
    // to it. Therefore, to ensure that it doesn't try to access the stack-
    // allocated `arg` after we return from this function, we need to NULL out
    // its state before releasing it.
    CHECK(BlimpObject_SetExtensionState(visitor, NULL));
    BlimpObject_Release(visitor);
    if (ret != BLIMP_OK) {
        goto error;
    }

    if (Vector_Empty(&arg.trees)) {
        Vector_Destroy(&arg.trees);
        // No sub-trees, this object represents a terminal. We do not need to
        // reparse terminals, we can just construct them directly.
        ret = ParseTree_New(blimp, symbol, NULL, 0, NULL, tree);
        BlimpObject_Release(symbol);
        return ret;
    } else {
        // This object represents a non-terminal via a sequence of sub-trees
        // which must be reparsed to create a single tree. Non-terminals are
        // required to be symbols.
        const Symbol *sym;
        if (BlimpObject_ParseSymbol(symbol, &sym) != BLIMP_OK) {
            BlimpObject_Release(symbol);
            goto error;
        }
        NonTerminal nt;
        if (Grammar_GetNonTerminal(&blimp->grammar, sym, &nt) != BLIMP_OK) {
            goto error;
        }
        if (Reparse(&arg.trees, &blimp->grammar, nt, NULL, tree) != BLIMP_OK) {
            goto error;
        }
        Vector_Destroy(&arg.trees);

        return BLIMP_OK;
    }

error:
    Vector_Destroy(&arg.trees);

    // Make sure `tree` is a valid parse tree even if we failed.
    TRY(Blimp_GetSymbol(blimp, "", (const Symbol **)&symbol));
    TRY(ParseTree_New(blimp, symbol, NULL, 0, NULL, tree));
    return Reraise(blimp);
}

// Method handler for the `parse` intrinsic.
static Status ParseMethod(
    Blimp *blimp,
    Object *context,
    Object *receiver,
    Object *message,
    Object **result)
{
    (void)context;
    (void)receiver;

    // Convert the object to a parse tree and reparse it.
    ParseTree *tree;
    TRY(ParseObject(blimp, message, &tree));

    // Create an extension object to represent the resulting tree via the parse
    // tree protocol.
    if (BlimpObject_NewExtension(
            blimp,
            (Object *)blimp->global,
            tree,
            ParseTreeMethod,
            ParseTreeFinalizer,
            result
    ) != BLIMP_OK)
    {
        ParseTree_Release(tree);
        return Reraise(blimp);
    }

    return BLIMP_OK;
}

Status InitParseIntrinsic(Blimp *blimp)
{
    const Symbol *parse_sym;
    TRY(Blimp_GetSymbol(blimp, "parse", &parse_sym));

    Object *parse_obj;
    TRY(BlimpObject_NewExtension(
        blimp, (Object *)blimp->global, NULL, ParseMethod, NULL, &parse_obj));
    if (BlimpObject_Set((Object *)blimp->global, parse_sym, parse_obj)
            != BLIMP_OK)
    {
        BlimpObject_Release(parse_obj);
        return Reraise(blimp);
    }
    BlimpObject_Release(parse_obj);

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
    if (sub_tree->grammar_symbol.is_terminal) {
        return sub_tree->grammar_symbol.terminal == t;
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
    if (sub_tree->grammar_symbol.is_terminal) {
        assert(Object_Type(sub_tree->symbol) == OBJ_SYMBOL);
        return (const Symbol *)sub_tree->symbol;
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

    if (tree->num_sub_trees == 0) {
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

    return ErrorFrom(blimp, tree->range, BLIMP_INVALID_PARSE_TREE,
        "invalid parse tree");
}

static Status ParseTreeNodeToExpr(
    Blimp *blimp, const ParseTreeNode *node, Expr *left, Expr *right, Expr **expr)
{
    const ParseTree *tree = node->tree;

    switch (node->type) {
        case PARSE_TREE_TERMINAL:
            return BlimpExpr_NewObject(blimp, tree->symbol, expr);
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
            return BlimpExpr_NewMacro(blimp, left, right, expr);
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
