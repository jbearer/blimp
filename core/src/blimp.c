#include "internal/blimp.h"
#include "internal/eval.h"
#include "internal/expr.h"
#include "internal/symbol.h"

static Status DefaultGrammar(Blimp *blimp, Grammar *grammar);

Blimp *Blimp_New(const BlimpOptions *options)
{
    if (options == NULL) {
        options = &DEFAULT_BLIMP_OPTIONS;
    }

    Blimp *blimp = malloc(sizeof(Blimp));
    if (blimp == NULL) {
        goto err_malloc;
    }
    blimp->options = *options;
    blimp->counter = 0;
    memset(&blimp->last_error, 0, sizeof(blimp->last_error));

    if (DefaultGrammar(blimp, &blimp->grammar) != BLIMP_OK) {
        goto err_grammar;
    }

    if (SymbolTable_Init(blimp, &blimp->symbols) != BLIMP_OK) {
        goto err_symbols;
    }

    if (ObjectPool_Init(blimp, &blimp->objects) != BLIMP_OK) {
        goto err_objects;
    }

    if (Stack_Init(blimp, &blimp->stack) != BLIMP_OK) {
        goto err_stack;
    }

    if (ObjectStack_Init(blimp, &blimp->result_stack,
                blimp->options.recursion_limit * 16)
                    // We assume (somewhat arbitrarily) that there will be an
                    // average of 16 objects on the stack at a time per call
                    // stack frame. Errors in the size of the object stack will
                    // be caught and turned into BLIMP_STACK_OVERFLOW runtime
                    // errors.
            != BLIMP_OK)
    {
        goto err_result_stack;
    }

    if (Optimizer_Init(blimp, &blimp->optimizer) != BLIMP_OK) {
        goto err_optimizer;
    }

    // Create the global object.
    if (GlobalObject_New(blimp, &blimp->global) != BLIMP_OK) {
        goto err_global;
    }

    InitSignals(blimp, &blimp->signals);

    return blimp;

err_global:
    Optimizer_Destroy(&blimp->optimizer);
err_optimizer:
    ObjectStack_Destroy(blimp, &blimp->result_stack);
err_result_stack:
    Stack_Destroy(blimp, &blimp->stack);
err_stack:
    ObjectPool_Destroy(&blimp->objects);
err_objects:
err_symbols:
    Grammar_Destroy(&blimp->grammar);
err_grammar:
    free(blimp);
err_malloc:
    return NULL;
}

void Blimp_Delete(Blimp *blimp)
{
    Optimizer_Destroy(&blimp->optimizer);
    ObjectStack_Destroy(blimp, &blimp->result_stack);
    Stack_Destroy(blimp, &blimp->stack);
    ObjectPool_Destroy(&blimp->objects);
    Grammar_Destroy(&blimp->grammar);
    SymbolTable_Destroy(&blimp->symbols);
    free(blimp);
}

Status Blimp_GetSymbol(Blimp *blimp, const char *name, const Symbol **symbol)
{
    return SymbolTable_GetSymbol(&blimp->symbols, name, symbol);
}

Object *Blimp_GlobalObject(Blimp *blimp)
{
    return (Object *)blimp->global;
}

Bytecode *Blimp_GlobalBytecode(Blimp *blimp)
{
    const StackFrame *frame = Stack_BottomFrame(&blimp->stack);
    if (frame == NULL) {
        return NULL;
    } else {
        return frame->executing;
    }
}

////////////////////////////////////////////////////////////////////////////////
// Parser API
//

#define UNPAREN(...)__VA_ARGS__
#define APPLY(x) x
#define ARRAY(ELEMS) {APPLY(UNPAREN ELEMS)}

#define ADD_GRAMMAR_RULE(GRAMMAR, NT, SYMBOLS, HANDLER, ARG) \
    Grammar_AddRule( \
        GRAMMAR, \
        NT, \
        sizeof((GrammarSymbol[])ARRAY(SYMBOLS))/sizeof(GrammarSymbol), \
        (GrammarSymbol[])ARRAY(SYMBOLS), HANDLER, ARG)

#define CONCAT(x, y) x ## y
#define T(TERMINAL) {.is_terminal = true, .terminal = CONCAT(TOK_,TERMINAL)}
#define NT(NON_TERMINAL) {.is_terminal = false, .non_terminal = NON_TERMINAL}

// Use an existing sub-tree as the result of a reduction to a lower-precedence
// non-terminal. The index of the sub-tree to use, relative to the symbols
// matched by the production which triggered this reduction, is `ctx->arg`.
static Status IdentityHandler(
    ParserContext *ctx, Expr **sub_exprs, Expr **result)
{
    *result = BlimpExpr_Borrow(sub_exprs[(size_t)ctx->arg]);
    return BLIMP_OK;
}

// Handler for `Expr -> Stmt ; Expr`
static Status SequenceHandler(
    ParserContext *ctx, Expr **sub_exprs, Expr **result)
{
    (void)ctx;

    // The sub-statement may already be a sequence, so combining it with the
    // sub-expression amounts to appending two linked lists.
    sub_exprs[0]->last->next = BlimpExpr_Borrow(sub_exprs[2]);
    sub_exprs[0]->last = sub_exprs[2]->last;
    *result = BlimpExpr_Borrow(sub_exprs[0]);
    return BLIMP_OK;
}

// Handler for `Stmt -> Stmt -> Expr`
static Status SendHandler(
    ParserContext *ctx, Expr **sub_exprs, Expr **result)
{
    return BlimpExpr_NewSend(ctx->blimp, sub_exprs[0], sub_exprs[1], result);
}

// Handler for `Term -> {^msg Expr}` and `Term -> {Expr}`. `ctx->arg` should be
// non-zero if an explicit message name for the block is provided (first case)
// and zero otherwise (second case).
static Status BlockHandler(
    ParserContext *ctx, Expr **sub_exprs, Expr **result)
{
    bool has_msg_name = (bool)ctx->arg;

    Expr *body = sub_exprs[1];

    const Symbol *msg_name = NULL;
    if (has_msg_name) {
        assert(sub_exprs[1]->tag == EXPR_TOKEN);
        assert(sub_exprs[1]->tok.type == TOK_MSG_NAME);
        msg_name = sub_exprs[1]->tok.symbol;
        body = sub_exprs[2];
            // The message name symbol offsets the index of the body symbol by
            // 1.
    }

    return BlimpExpr_NewBlock(ctx->blimp, msg_name, body, result);
}

// Handler for `Term -> symbol`
static Status SymbolHandler(
    ParserContext *ctx, Expr **sub_exprs, Expr **result)
{
    assert(sub_exprs[0]->tag == EXPR_TOKEN);
    assert(sub_exprs[0]->tok.type == TOK_SYMBOL);
    return BlimpExpr_NewSymbol(ctx->blimp, sub_exprs[0]->tok.symbol, result);
}

// Handler for `Term -> ^msg`
static Status MsgHandler(
    ParserContext *ctx, Expr **sub_exprs, Expr **result)
{
    assert(sub_exprs[0]->tag == EXPR_TOKEN);
    assert(sub_exprs[0]->tok.type == TOK_MSG_NAME);
    return BlimpExpr_NewMsgName(ctx->blimp, sub_exprs[0]->tok.symbol, result);
}

// Handler for `Term -> ^`
static Status MsgThisHandler(
    ParserContext *ctx, Expr **sub_exprs, Expr **result)
{
    (void)sub_exprs;

    return BlimpExpr_NewMsgIndex(ctx->blimp, 0, result);
}

static Status DefaultGrammar(Blimp *blimp, Grammar *grammar)
{
    TRY(Grammar_Init(blimp, grammar, TOK_EOF));

    // The bl:mp grammar is fundamentally simple. We have five kinds of
    // expressions:
    //  1. Sequences        A; B
    //  2. Sends            A B
    //  3. Blocks           {^msg A}
    //  4. Symbols          sym
    //  5. Message names    ^msg
    //
    // These are conceptually divided into expressions, statements, and terms to
    // indicate increasing precedence:
    //
    //  Expr -> Stmt ; Expr | Stmt
    //  Stmt -> Stmt Expr | Term
    //  Term -> {^msg Expr}
    //        | sym
    //        | ^msg
    //        | ( Expr )
    //
    // Unfortunately, the ^msg part of the block syntax is optional, and this
    // complicates everything. Consider {^msg foo}. This could be parsed as a
    // block with a message name specified, whose body expression is a single
    // symbol `foo`. Or it could be parsed as a block with no message name whose
    // body expression is a send `^msg foo` (where `^msg` is acting not as the
    // message name for the block but as a term on its own). The rule is that
    // the former takes precedence. In order to encode this rule, we need to
    // factorize the grammar so we can express that a block either begins with a
    // message name follwed by an expression, or without a message name and an
    // expression _that does not start with a message name_:
    //
    //  Expr      -> Stmt ; Expr
    //             | Stmt
    //  ExprNoMsg -> StmtNoMsg ; Expr
    //             | StmtNoMsg
    //  Stmt      -> Stmt Expr
    //             | Term
    //  StmtNoMsg -> StmtNoMsg Expr
    //             | TermNoMsg
    //  Term      -> TermNoMsg | ^msg
    //  TermNoMsg -> {^msg Expr}
    //             | {ExprNoMsg}
    //             | sym
    //             | ( Expr )
    //
    // Here, the "NoMsg" non-terminals represent everything from the language of
    // the corresponding non-terminal except sentences that start with a ^msg
    // token.
    //
    // Finally, there is one more factorization we need to do. We want the
    // parser to update itself with new productions added by macros at least as
    // often as in between top-level statements; that is, after parsing
    // `Stmt ;` or `StmtNoMsg ;`. Unfortunately, at such a point in parsing, the
    // presence of `;` (which, being a terminal, has higher precedence than any
    // non-terminal) in the output precludes us from being able to add new
    // productions that start with a non-terminal (even if that non-terminal is
    // relatively high precedence, like Term). To fix this, we force the parser
    // to reduce `;` into a low-precedence non-terminal as soon as it sees it,
    // by adding the rule `Semi -> ;`, where `Semi` has precedence just higher
    // than StmtNoMsg. We then replace the two productions that used the `;`
    // terminal with:
    //
    //  Expr      -> Stmt Semi Expr
    //  ExprNoMsg -> StmtNoMsg Semi Expr
    //
    // Now, after parsing a top-level statement, the output consists only of two
    // non-terminals `Stmt Semi`, both of which have lower precedence than most
    // non-terminals that we would find in a newly added rule.
    //
    // Here are our non-terminals in order of ascending precedence:
    enum {
        Start,
        Expr,
        ExprNoMsg,
        Stmt,
        StmtNoMsg,
        Semi,
        Term,
        TermNoMsg,
    };

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

    // Give the parser readable names for all of the terminals.
    for (TokenType t = 0; t < NUM_TOKEN_TYPES; ++t) {
        Grammar_SetTerminalString(grammar, t, StringOfTokenType(t));
    }

    // Give the parser readable names for all of the non-terminals.
    Grammar_SetNonTerminalString(grammar, Start, "Start");
    Grammar_SetNonTerminalString(grammar, Expr, "E");
    Grammar_SetNonTerminalString(grammar, ExprNoMsg, "E'");
    Grammar_SetNonTerminalString(grammar, Stmt, "S");
    Grammar_SetNonTerminalString(grammar, StmtNoMsg, "S'");
    Grammar_SetNonTerminalString(grammar, Semi, "Semi");
    Grammar_SetNonTerminalString(grammar, Term, "T");
    Grammar_SetNonTerminalString(grammar, TermNoMsg, "T'");

    return BLIMP_OK;
}

Status Blimp_Parse(Blimp *blimp, Stream *input, Expr **output)
{
    Lexer lex;
    Lexer_Init(&lex, blimp, input);

    Status ret = Parse(&lex, &blimp->grammar, NULL, output);
    Lexer_Destroy(&lex);
    Stream_Delete(input);
    TRY(ret);

    return BlimpExpr_Resolve(blimp, *output);
}

Status Blimp_ParseFile(Blimp *blimp, const char *path, Expr **output)
{
    Stream *stream;
    TRY(Blimp_FileStream(blimp, path, &stream));
    return Blimp_Parse(blimp, stream, output);
}

Status Blimp_ParseString(Blimp *blimp, const char *str, Expr **output)
{
    Stream *stream;
    TRY(Blimp_StringStream(blimp, str, &stream));
    return Blimp_Parse(blimp, stream, output);
}

void Blimp_DumpGrammarVitals(FILE *file, Blimp *blimp)
{
    Grammar_DumpVitals(file, &blimp->grammar);
}

////////////////////////////////////////////////////////////////////////////////
// Object API
//

Status BlimpObject_NewBlock(
    Blimp *blimp,
    Object *parent,
    const Symbol *msg_name,
    Expr *code,
    Object **obj)
{
    if (!IsScopedObject(parent)) {
        return ErrorMsg(
            blimp, BLIMP_INVALID_OBJECT_TYPE,
            "parent of block must be block or extension");
    }

    Bytecode *bytecode;
    TRY(BlimpExpr_Compile(blimp, code, &bytecode));

    return BlockObject_New(
        blimp,
        (ScopedObject *)parent,
        msg_name,
        bytecode,
        0,
        (BlockObject **)obj);
}

Status BlimpObject_NewExtension(
    Blimp *blimp,
    Object *parent,
    void *state,
    BlimpMethod method,
    BlimpFinalizer finalize,
    Object **obj)
{
    if (!IsScopedObject(parent)) {
        return ErrorMsg(
            blimp, BLIMP_INVALID_OBJECT_TYPE,
            "parent of extension must be block or extension");
    }

    return ExtensionObject_New(
        blimp,
        (ScopedObject *)parent,
        state,
        method,
        finalize,
        (ExtensionObject **)obj
    );
}

Status BlimpObject_NewSymbol(Blimp *blimp, const Symbol *sym, Object **obj)
{
    (void)blimp;

    *obj = (Object *)sym;
        // A Symbol is already an Object, so this is just an upcast.

    return BLIMP_OK;
}

void BlimpObject_Print(FILE *f, const Object *obj)
{
    Blimp *blimp = Object_Blimp(obj);

    switch (Object_Type(obj)) {
        case OBJ_SYMBOL:
            fputs(((const Symbol *)obj)->name, f);
            break;
        case OBJ_BLOCK: {
            DeBruijnMap scopes;
            DBMap_Init(blimp, &scopes);

            for (const ScopedObject *cur = (const ScopedObject *)obj;
                 cur != NULL;
                 cur = cur->parent)
            {
                size_t i = 0;
                if (Object_Type((Object *)cur) == OBJ_BLOCK) {
                    BlockObject *block = (BlockObject *)cur;
                    DBMap_Shift(&scopes, (void *)block->msg_name);
                    ++i;
                }

                for (; i < cur->owned_captures; ++i) {
                    DBMap_Shift(&scopes, NULL);
                }
            }

            fprintf(f, "{^%s ", ((BlockObject *)obj)->msg_name->name);
            PrintClosure(
                f, Bytecode_Expr(((BlockObject *)obj)->code), &scopes);
            fprintf(f, "}");

            DBMap_Destroy(&scopes);
            break;
        }
        case OBJ_EXTENSION:
            fprintf(f, "<extension>");
            break;
        case OBJ_REFERENCE:
            fprintf(f, "<ref:%s>", ((ReferenceObject *)obj)->symbol->name);
            break;
        case OBJ_GLOBAL:
            fprintf(f, "<global>");
            break;
        default:
            assert(false);
    }
}

Status BlimpObject_ParseBlock(const Object *obj, Bytecode **code)
{
    if (Object_Type(obj) != OBJ_BLOCK) {
        return Error(Object_Blimp(obj), BLIMP_MUST_BE_BLOCK);
    }

    if (code) {
        *code = ((BlockObject *)obj)->code;
    }
    return BLIMP_OK;
}

Status BlimpObject_ParseExtension(
    const Object *obj, BlimpMethod *method, void **state)
{
    if (Object_Type(obj) != OBJ_EXTENSION) {
        return Error(Object_Blimp(obj), BLIMP_MUST_BE_EXTENSION);
    }

    if (method) {
        *method = ((ExtensionObject *)obj)->method;
    }
    if (state) {
        *state = ((ExtensionObject *)obj)->state;
    }
    return BLIMP_OK;
}

Status BlimpObject_ParseSymbol(const Object *obj, const Symbol **sym)
{
    if (Object_Type(obj) != OBJ_SYMBOL) {
        return Error(Object_Blimp(obj), BLIMP_MUST_BE_SYMBOL);
    }

    if (sym) {
        *sym = (const Symbol *)obj;
    }
    return BLIMP_OK;
}


Status BlimpObject_Get(Object *obj, const Symbol *sym, Object **ret)
{
    assert(Object_Type(obj) != OBJ_FREE);

    if (!IsScopedObject(obj)) {
        return ErrorMsg(Object_Blimp(obj), BLIMP_INVALID_OBJECT_TYPE,
            "cannot get from non-scoped object");
    }

    return ScopedObject_Get((ScopedObject *)obj, sym, ret);
}

Status BlimpObject_Set(Object *obj, const Symbol *sym, Object *val)
{
    assert(Object_Type(obj) != OBJ_FREE);
    assert(Object_Type(val) != OBJ_FREE);

    if (!IsScopedObject(obj)) {
        return ErrorMsg(Object_Blimp(obj), BLIMP_INVALID_OBJECT_TYPE,
            "cannot set in non-scoped object");
    }

    return ScopedObject_Set((ScopedObject *)obj, sym, val);
}

Status BlimpObject_CaptureMessage(Object*obj, Object *message)
{
    if (!IsScopedObject(obj)) {
        return ErrorMsg(Object_Blimp(obj), BLIMP_INVALID_OBJECT_TYPE,
            "non-scoped object cannot capture a message");
    }

    return ScopedObject_CaptureMessage((ScopedObject *)obj, message);
}

Status BlimpObject_GetCapturedMessage(
    Object *obj, size_t index, Object **message)
{
    if (!IsScopedObject(obj)) {
        return ErrorMsg(Object_Blimp(obj), BLIMP_INVALID_OBJECT_TYPE,
            "cannot get captured message from non-scoped object");
    }

    return ScopedObject_GetCapturedMessage((ScopedObject *)obj, index, message);
}

Status BlimpObject_GetCapturedMessageByName(
    Object *obj, const Symbol *name, Object **message)
{
    if (!IsScopedObject(obj)) {
        return ErrorMsg(Object_Blimp(obj), BLIMP_INVALID_OBJECT_TYPE,
            "cannot get captured message from non-scoped object");
    }

    return ScopedObject_GetCapturedMessageByName(
        (ScopedObject *)obj, name, message);
}

////////////////////////////////////////////////////////////////////////////////
// Evaluation
//

Status Blimp_Eval(
    Blimp *blimp,
    Expr *expr,
    Object *scope,
    Object **obj)
{
    if (!IsScopedObject(scope)) {
        return Blimp_ErrorMsg(blimp, BLIMP_INVALID_OBJECT_TYPE,
            "evaluation scope must be a block or extension");
    }

    Bytecode *code;
    TRY(BlimpExpr_Compile(blimp, expr, &code));

    Status ret = EvalBytecode(blimp, (ScopedObject *)scope, code, obj);
    BlimpBytecode_Free(code);

    return ret;
}

Status Blimp_EvalSymbol(
    Blimp *blimp,
    Expr *expr,
    Object *scope,
    const Symbol **sym)
{
    Object *obj;
    TRY(Blimp_Eval(blimp, expr, scope, &obj));

    Status ret = BlimpObject_ParseSymbol(obj, sym);
    BlimpObject_Release(obj);

    return ret;
}

BlimpStatus Blimp_Send(
    Blimp *blimp,
    Object *scope,
    Object *receiver,
    Object *message,
    Object **result)
{
    if (!IsScopedObject(scope)) {
        return Blimp_ErrorMsg(blimp, BLIMP_INVALID_OBJECT_TYPE,
            "send scope must be a block or extension");
    }

    return Send(blimp, (ScopedObject *)scope, receiver, message, result);
}

BlimpStatus Blimp_SendAndParseSymbol(
    Blimp *blimp,
    Object *scope,
    Object *receiver,
    Object *message,
    const Symbol **sym)
{
    Object *obj;
    TRY(Blimp_Send(blimp, scope, receiver, message, &obj));

    BlimpStatus status = BlimpObject_ParseSymbol(obj, sym);
    BlimpObject_Release(obj);

    return status;
}

BlimpGCStatistics Blimp_GetGCStatistics(Blimp *blimp)
{
    return ObjectPool_GetStats(&blimp->objects);
}

void Blimp_DumpHeap(FILE *f, Blimp *blimp)
{
    ObjectPool_DumpHeap(f, &blimp->objects, true);
}

void Blimp_DumpUnreachable(FILE *f, Blimp *blimp)
{
    ObjectPool_DumpHeap(f, &blimp->objects, false);
}

void Blimp_CollectGarbage(Blimp *blimp)
{
    ObjectPool_CollectGarbage(&blimp->objects);
}
