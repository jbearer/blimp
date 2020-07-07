#include "internal/blimp.h"
#include "internal/expr.h"
#include "internal/symbol.h"
#include "internal/vtable.h"

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

    if (SymbolTable_Init(blimp, &blimp->symbols) != BLIMP_OK) {
        goto err_symbols;
    }

    if (Blimp_GetSymbol(blimp, "this", &blimp->this_symbol) != BLIMP_OK) {
        goto err_this_and_that;
    }
    if (Blimp_GetSymbol(blimp, "that", &blimp->that_symbol) != BLIMP_OK) {
        goto err_this_and_that;
    }

    if (ObjectPool_Init(blimp, &blimp->objects) != BLIMP_OK) {
        goto err_objects;
    }

    if (VTable_Init(blimp, &blimp->vtable) != BLIMP_OK) {
        goto err_vtable;
    }

    if (Stack_Init(blimp, &blimp->stack) != BLIMP_OK) {
        goto err_stack;
    }

    // Create the global object.
    if (BlimpObject_NewGlobal(blimp, &blimp->global) != BLIMP_OK) {
        goto err_global;
    }

    return blimp;

err_global:
    Stack_Destroy(blimp, &blimp->stack);
err_stack:
    VTable_Destroy(&blimp->vtable);
err_vtable:
    ObjectPool_Destroy(blimp, &blimp->objects);
err_objects:
err_this_and_that:
    SymbolTable_Destroy(&blimp->symbols);
err_symbols:
    free(blimp);
err_malloc:
    return NULL;
}

void Blimp_Delete(Blimp *blimp)
{
    Stack_Destroy(blimp, &blimp->stack);
    VTable_Destroy(&blimp->vtable);
    ObjectPool_Destroy(blimp, &blimp->objects);
    SymbolTable_Destroy(&blimp->symbols);
    free(blimp);
}

Status Blimp_GetSymbol(Blimp *blimp, const char *name, const Symbol **symbol)
{
    return SymbolTable_GetSymbol(&blimp->symbols, name, symbol);
}

Object *Blimp_GlobalObject(Blimp *blimp)
{
    return blimp->global;
}

////////////////////////////////////////////////////////////////////////////////
// Evaluation
//

static const Method BLIMP_METHOD_EVAL = NULL;
    // Sentinel method indicating that this method is bound to a bl:mp
    // expression. Having this information available directly in `Send` is a bit
    // more convenient than binding the method to BlimpMethod_Eval, because we
    // can preserve the result type of the message send and potentially avoid
    // the creation of temporary objects.

// Dynamic result types. This structure allows the caller of an evaluation
// function to request a specific kind of result from the callee, by setting the
// `type` field before passing a pointer to a `Value`. Depending on the
// requested result type, the callee may be able to evaluate the expression more
// efficiently. For example, if the caller requests a symbol, and the expression
// is an EXPR_SYMBOL, the callee doesn't need to ever create an object
// representing the result of evaluating the expression. They can pass the
// symbol back to the caller directly.
typedef struct {
    enum {
        VALUE_OBJECT,
        VALUE_SYMBOL,
        VALUE_VOID,
    } type;

    union {
        Object *obj;
        const Symbol *sym;
    };
} Value;

// Initialize a `Value` union with a symbol literal, converting to an object if
// requested by the creator of the `Value`.
static inline Status ReturnSymbol(
    Blimp *blimp,
    Object *scope,
    const Symbol *sym,
    Value *result)
{
    switch (result->type) {
        case VALUE_OBJECT:
            return BlimpObject_NewSymbol(blimp, scope, sym, &result->obj);

        case VALUE_SYMBOL:
            result->sym = sym;
            return BLIMP_OK;

        case VALUE_VOID:
            return BLIMP_OK;

        default:
            assert(false);
            return Error(blimp, BLIMP_INVALID_EXPR);
    }
}

// Initialize a `Value` union with a block literal, creating a new object only
// if requested by the creator of the `Value`.
static inline Status ReturnBlock(
    Blimp *blimp,
    Object *scope,
    const Symbol *tag,
    Expr *code,
    Value *result)
{
    switch (result->type) {
        case VALUE_OBJECT:
            return BlimpObject_NewBlock(blimp, scope, tag, code, &result->obj);

        case VALUE_SYMBOL:
            return Error(blimp, BLIMP_MUST_BE_SYMBOL);

        case VALUE_VOID:
            return BLIMP_OK;

        default:
            assert(false);
            return Error(blimp, BLIMP_INVALID_EXPR);
    }
}

// Initialize a `Value` union with an object, parsing it as a symbol if
// requested by the creator of the `Value`.
//
// This function consumes the caller's reference to the object. If the object
// is stored in `result`, its reference count will not be incremented. If the
// creator of `result` did not request an object, then a reference to `obj` will
// be released.
static inline Status ReturnObject(Blimp *blimp, Object *obj, Value *result)
{
    (void)blimp;

    switch (result->type) {
        case VALUE_OBJECT:
            result->obj = obj;
            return BLIMP_OK;

        case VALUE_SYMBOL: {
            Status ret = BlimpObject_ParseSymbol(obj, &result->sym);
            BlimpObject_Release(obj);

            if (ret != BLIMP_OK) {
                return RuntimeReraise(blimp);
            } else {
                return BLIMP_OK;
            }
        }

        case VALUE_VOID:
            BlimpObject_Release(obj);
            return BLIMP_OK;

        default:
            assert(false);
            return Error(blimp, BLIMP_INVALID_EXPR);
    }
}

static Status EvalExpr(
    Blimp *blimp, const Expr *expr, Object *scope, Value *result);
static Status EvalStmt(
    Blimp *blimp, const Expr *expr, Object *scope, Value *result);
static Status Send(
    Blimp *blimp,
    Object *scope,
    Object *receiver,
    Object *message,
    Value *result);

static Status EvalExpr(
    Blimp *blimp, const Expr *expr, Object *scope, Value *result)
{
    // Evaluate all expressions except the last in the sequence of expressions.
    // We don't care about the results here, this is just for side-effects.
    while (expr->next) {
        TRY(EvalStmt(blimp, expr, scope, &(Value){.type=VALUE_VOID}));
        expr = expr->next;
    }

    // Evaluate the final expression in the sequence for its result.
    return EvalStmt(blimp, expr, scope, result);
}

static Status EvalStmt(
    Blimp *blimp,
    const Expr *expr,
    Object *scope,
    Value *result)
{
    switch (expr->tag) {
        case EXPR_SYMBOL:
            return ReturnSymbol(blimp, scope, expr->symbol, result);

        case EXPR_BLOCK: {
            Value tag = {.type=VALUE_SYMBOL};
            TRY(EvalExpr(blimp, expr->block.tag, scope, &tag));
            return ReturnBlock(blimp, scope, tag.sym, expr->block.code, result);
        }

        case EXPR_BIND: {
            Value receiver = {.type=VALUE_SYMBOL};
            Value message  = {.type=VALUE_SYMBOL};

            TRY(EvalExpr(blimp, expr->bind.receiver, scope, &receiver));
            TRY(EvalExpr(blimp, expr->bind.message,  scope, &message));
            TRY(Blimp_BindExpr(
                blimp, receiver.sym, message.sym, expr->bind.code));

            return ReturnSymbol(blimp, scope, receiver.sym, result);
        }

        case EXPR_SEND: {
            Status ret = BLIMP_OK;

            Value receiver = {VALUE_OBJECT};
            Value message  = {VALUE_OBJECT};

            if ((ret = EvalExpr(blimp, expr->send.receiver, scope, &receiver))
                    != BLIMP_OK)
            {
                goto err_receiver;
            }

            if ((ret = EvalExpr(blimp, expr->send.message, scope, &message))
                    != BLIMP_OK)
            {
                goto err_message;
            }

            StackFrame frame = {
                .range = expr->range,
            };
            if ((ret = Stack_Push(blimp, &blimp->stack, &frame, 128)) != BLIMP_OK)
            {
                goto err_push;
            }

            if ((ret = Send(blimp, scope, receiver.obj, message.obj, result))
                    != BLIMP_OK)
            {
                goto err_send;
            }

err_send:
            Stack_Pop(blimp, &blimp->stack);
err_push:
            BlimpObject_Release(message.obj);
err_message:
            BlimpObject_Release(receiver.obj);
err_receiver:
            if (ret && !ret->has_range) {
                // A lot of specialized method handlers do not assign source.
                // locations to their errors. If this was one of those methods,
                // set the location to the location of the overal expression.
                ret->range = expr->range;
                ret->has_range = true;
            }

            return ret;
        }

        default:
            assert(false);
            return ErrorFrom(
                blimp,
                expr->range,
                BLIMP_INVALID_EXPR,
                "internal error: expression is invalid");
    }
}

static Status Send(
    Blimp *blimp,
    Object *scope,
    Object *receiver,
    Object *message,
    Value *result)
{
    const Symbol *receiver_tag = BlimpObject_Tag(receiver);
    const Symbol *message_tag  = BlimpObject_Tag(message);

    Method method;
    void *data;
    TRY(VTable_Resolve(
        &blimp->vtable, receiver_tag, message_tag, &method, &data));

    if (method == BLIMP_METHOD_EVAL) {
        TRY(BlimpObject_Set(receiver, blimp->this_symbol, receiver));
        TRY(BlimpObject_Set(receiver, blimp->that_symbol, message));
        return EvalExpr(blimp, (const Expr *)data, receiver, result);
    } else {
        Object *obj;
        TRY(method(blimp, scope, receiver, message, data, &obj));
        return ReturnObject(blimp, obj, result);
    }
}

Status Blimp_Bind(
    Blimp *blimp,
    const Symbol *receiver,
    const Symbol *message,
    const Method method,
    void *data)
{
    return VTable_Bind(&blimp->vtable, receiver, message, method, data);
}

Status Blimp_BindVTableFragment(Blimp *blimp, BlimpVTableFragment fragment)
{
    for (BlimpVTableEntry *entry = fragment; entry->receiver; ++entry) {
        const Symbol *receiver;
        TRY(Blimp_GetSymbol(blimp, entry->receiver, &receiver));

        const Symbol *message;
        TRY(Blimp_GetSymbol(blimp, entry->message, &message));

        TRY(Blimp_Bind(blimp, receiver, message, entry->method, entry->data));
    }

    return BLIMP_OK;
}

Status Blimp_BindExpr(
    Blimp *blimp,
    const Symbol *receiver,
    const Symbol *message,
    Expr *expr)
{
    ++expr->refcount;
    return VTable_Bind(
        &blimp->vtable,
        receiver,
        message,
        BLIMP_METHOD_EVAL,
        (void *)expr
    );
}

Status Blimp_Resolve(
    Blimp *blimp,
    const Symbol *receiver,
    const Symbol *message,
    Method *method,
    void **data)
{
    return VTable_Resolve(&blimp->vtable, receiver, message, method, data);
}

Status Blimp_Send(
    Blimp *blimp,
    Object *context,
    Object *receiver,
    Object *message,
    Object **result)
{
    Value v = {VALUE_OBJECT};
    TRY(Send(blimp, context, receiver, message, &v));
    *result = v.obj;
    return BLIMP_OK;
}

Status BlimpMethod_Eval(
    Blimp *blimp,
    Object *context,
    Object *receiver,
    Object *message,
    const Expr *body,
    Object **result)
{
    (void)context;

    TRY(BlimpObject_Set(receiver, blimp->this_symbol, receiver));
    TRY(BlimpObject_Set(receiver, blimp->that_symbol, message));
    return Blimp_Eval(blimp, body, receiver, result);
}

Status BlimpMethod_PrimitiveGet(
    Blimp *blimp,
    Object *context,
    Object *receiver,
    Object *message,
    void *data,
    Object **result)
{
    (void)blimp;
    (void)message;
    (void)data;

    const Symbol *sym;
    TRY(BlimpObject_ParseSymbol(receiver, &sym));
    TRY(BlimpObject_Get(context, sym, result));
    BlimpObject_Borrow(*result);
    return BLIMP_OK;
}

Status BlimpMethod_PrimitiveSet(
    Blimp *blimp,
    Object *context,
    Object *receiver,
    Object *message,
    void *data,
    Object **result)
{
    (void)data;

    const Symbol *sym;
    TRY(BlimpObject_ParseSymbol(receiver, &sym));

    TRY(BlimpObject_Eval(message, result));
    if (BlimpObject_Set(context, sym, *result) != BLIMP_OK) {
        BlimpObject_Release(*result);
        return Blimp_Reraise(blimp);
    }

    return BLIMP_OK;
}

Status BlimpMethod_PrimitiveEval(
    Blimp *blimp,
    Object *context,
    Object *receiver,
    Object *message,
    void *data,
    Object **result)
{
    (void)blimp;
    (void)context;
    (void)message;
    (void)data;

    return BlimpObject_Eval(receiver, result);
}

Status Blimp_Eval(
    Blimp *blimp,
    const Expr *expr,
    Object *scope,
    Object **obj)
{
    Value v = {VALUE_OBJECT};
    TRY(EvalExpr(blimp, expr, scope, &v));
    *obj = v.obj;
    return BLIMP_OK;
}

Status Blimp_EvalSymbol(
    Blimp *blimp,
    const Expr *expr,
    Object *scope,
    const Symbol **sym)
{
    Value v = {.type=VALUE_SYMBOL};
    TRY(EvalExpr(blimp, expr, scope, &v));
    *sym = v.sym;
    return BLIMP_OK;
}

BlimpGCStatistics Blimp_GetGCStatistics(Blimp *blimp)
{
    return ObjectPool_GetStats(&blimp->objects);
}

void Blimp_CollectGarbage(Blimp *blimp)
{
    ObjectPool_CollectGarbage(&blimp->objects);
}
