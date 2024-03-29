#include "internal/blimp.h"
#include "internal/eval.h"
#include "internal/expr.h"
#include "internal/grammar.h"
#include "internal/symbol.h"

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

    if (SymbolTable_Init(blimp, &blimp->symbols) != BLIMP_OK) {
        goto err_symbols;
    }
    SymbolMapAllocator_Init(blimp, &blimp->sym_alloc);

    if (TokenTrie_Init(blimp, &blimp->tokens) != BLIMP_OK) {
        goto err_tokens;
    }

    if (DefaultGrammar(blimp, &blimp->grammar) != BLIMP_OK) {
        goto err_grammar;
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

    if (InitParseIntrinsic(blimp) != BLIMP_OK) {
        goto err_parse;
    }

    InitSignals(blimp, &blimp->signals);

    return blimp;

err_parse:
err_global:
    Optimizer_Destroy(&blimp->optimizer);
err_optimizer:
    ObjectStack_Destroy(blimp, &blimp->result_stack);
err_result_stack:
    Stack_Destroy(blimp, &blimp->stack);
err_stack:
    ObjectPool_Destroy(&blimp->objects);
err_objects:
    Grammar_Destroy(&blimp->grammar);
err_grammar:
    TokenTrie_Destroy(&blimp->tokens);
err_tokens:
    SymbolMapAllocator_Destroy(&blimp->sym_alloc);
    SymbolTable_Destroy(&blimp->symbols);
err_symbols:
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
    TokenTrie_Destroy(&blimp->tokens);
    SymbolMapAllocator_Destroy(&blimp->sym_alloc);
    SymbolTable_Destroy(&blimp->symbols);
    free(blimp);
}

Status Blimp_GetSymbol(Blimp *blimp, const char *name, const Symbol **symbol)
{
    return SymbolTable_GetSymbol(&blimp->symbols, name, strlen(name), symbol);
}

Status Blimp_GetSymbolWithLength(
    Blimp *blimp, const char *name, size_t length, const Symbol **symbol)
{
    return SymbolTable_GetSymbol(&blimp->symbols, name, length, symbol);
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

Status Blimp_GetTerminal(Blimp *blimp, const Symbol *sym, Terminal *terminal)
{
    Token tok;
    TRY(TokenTrie_InsertToken(&blimp->tokens, sym->name, &tok));
    TRY(Grammar_AddTerminal(&blimp->grammar, tok.type));
        // Let the grammar know about the (possibly) newly created terminal.
    Grammar_SetTerminalString(&blimp->grammar, tok.type, tok.symbol->name);
        // Set a readable name for the new token.
    *terminal = tok.type;
    return BLIMP_OK;
}

Status Blimp_GetNonTerminal(
    Blimp *blimp, const Symbol *sym, NonTerminal *non_terminal)
{
    return Grammar_GetNonTerminal(&blimp->grammar, sym, non_terminal);
}

NonTerminal Blimp_DefaultNonTerminal(Blimp *blimp)
{
    // Get the top-level expression non-terminal;
    const Symbol *nt_symbol;
    CHECK(Blimp_GetSymbol(blimp, "_1", &nt_symbol));
    NonTerminal nt;
    CHECK(Grammar_GetNonTerminal(&blimp->grammar, nt_symbol, &nt));
    return nt;
}

Status Blimp_DefineMacro(
    Blimp *blimp,
    NonTerminal nt,
    GrammarSymbol *symbols,
    size_t num_symbols,
    BlimpMacroHandler handler,
    void *handler_arg)
{
    return Grammar_AddRule(
        &blimp->grammar,
        nt,
        num_symbols,
        symbols,
        handler,
        handler_arg);
}

Status Blimp_Parse(
    Blimp *blimp, Stream *input, NonTerminal nt, ParseTree **output)
{
    Lexer lex;
    Lexer_Init(&lex, blimp, input);

    Status ret = Parse(&lex, &blimp->grammar, nt, NULL, output);
    Lexer_Destroy(&lex);
    Stream_Delete(input);
    return ret;
}

Status Blimp_ParseFile(
    Blimp *blimp, const char *path, NonTerminal nt, ParseTree **output)
{
    Stream *stream;
    TRY(Blimp_FileStream(blimp, path, &stream));
    return Blimp_Parse(blimp, stream, nt, output);
}

Status Blimp_ParseString(
    Blimp *blimp, const char *str, NonTerminal nt, ParseTree **output)
{
    Stream *stream;
    TRY(Blimp_StringStream(blimp, str, &stream));
    return Blimp_Parse(blimp, stream, nt, output);
}

void Blimp_DumpGrammarVitals(FILE *file, Blimp *blimp)
{
    Grammar_DumpVitals(file, &blimp->grammar);
}

Status BlimpParseTree_Eval(Blimp *blimp, ParseTree *tree, Expr **expr)
{
    TRY(ParseTreeToExpr(blimp, tree, expr));
    if (BlimpExpr_Resolve(blimp, *expr) != BLIMP_OK) {
        Blimp_FreeExpr(*expr);
        return Reraise(blimp);
    }
    return BLIMP_OK;
}

void BlimpParseTree_Release(ParseTree *tree)
{
    ParseTree_Release(tree);
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
        BLOCK_USES_MESSAGE,
            // For API-created blocks, we don't know if the object will use its
            // message or not, so be conservative and say it does.
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

            BlockObject *block = (BlockObject *)obj;
            if (!(block->flags & BLOCK_MESSAGE_CAPTURED)) {
                fprintf(f, "{");
            } else {
                fprintf(f, "{^%s ", ((BlockObject *)obj)->msg_name->name);
            }
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

void BlimpObject_Dump(FILE *f, const Object *obj) {
    Blimp *blimp = Object_Blimp(obj);

    switch (Object_Type(obj)) {
        case OBJ_SYMBOL:
            fprintf(f, "|%s|", ((const Symbol *)obj)->name);
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

            BlockObject *block = (BlockObject *)obj;
            fprintf(f, "(obj |^%s|", block->msg_name->name);
            DumpClosure(f, Bytecode_Expr(block->code), &scopes);
            fprintf(f, ")");

            DBMap_Destroy(&scopes);
            break;
        }
        case OBJ_EXTENSION:
            fprintf(f, "(extension)");
            break;
        case OBJ_REFERENCE:
            fprintf(f, "(ref %s)", ((ReferenceObject *)obj)->symbol->name);
            break;
        case OBJ_GLOBAL:
            fprintf(f, "(global)");
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

Status BlimpObject_SetExtensionState(Object *obj, void *state)
{
    if (Object_Type(obj) != OBJ_EXTENSION) {
        return Error(Object_Blimp(obj), BLIMP_MUST_BE_EXTENSION);
    }

    ((ExtensionObject *)obj)->state = state;
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

Object *Blimp_CurrentScope(Blimp *blimp)
{
    const StackFrame *frame = Stack_CurrentFrame(&blimp->stack);
    if (frame == NULL) {
        return (Object *)blimp->global;
    } else {
        return (Object *)frame->scope;
    }
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
