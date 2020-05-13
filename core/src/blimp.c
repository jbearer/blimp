#include "internal/blimp.h"
#include "internal/expr.h"
#include "internal/symbol.h"
#include "internal/vtable.h"

BlimpOptions DEFAULT_BLIMP_OPTIONS = {
    .object_pool_batch_size = (1ull<<20), // 1MB
};

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

    // Create the global object.
    if (BlimpObject_NewGlobal(blimp, &blimp->global) != BLIMP_OK) {
        goto err_global;
    }

    return blimp;

err_global:
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
        (Method)BlimpMethod_Eval,
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
    const Symbol *receiver_tag = BlimpObject_Tag(receiver);
    const Symbol *message_tag  = BlimpObject_Tag(message);

    Method method;
    void *data;
    TRY(VTable_Resolve(
        &blimp->vtable, receiver_tag, message_tag, &method, &data));

    return method(blimp, context, receiver, message, data, result);
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
    Blimp *blimp, const Expr *expr, Object *scope, Object **result)
{
    switch (expr->tag) {
        case EXPR_SYMBOL:
            return BlimpObject_NewSymbol(blimp, scope, expr->symbol, result);

        case EXPR_BLOCK: {
            const Symbol *tag;
            if (Blimp_EvalSymbol(blimp, expr->block.tag, scope, &tag)
                    != BLIMP_OK)
            {
                return ErrorFrom(
                    blimp,
                    expr->block.tag->range,
                    BLIMP_MUST_BE_SYMBOL,
                    "object tag must evaluate to a symbol");
            }
            return BlimpObject_NewBlock(
                blimp, scope, tag, expr->block.code, result);
        }

        case EXPR_BIND: {
            TRY(Blimp_Eval(blimp, expr->bind.receiver, scope, result));

            const Symbol *receiver;
            if (BlimpObject_ParseSymbol(*result, &receiver) != BLIMP_OK) {
                return ErrorFrom(
                    blimp,
                    expr->bind.receiver->range,
                    BLIMP_MUST_BE_SYMBOL,
                    "bind receiver must evaluate to a symbol"
                );
            }

            const Symbol *message;
            if (Blimp_EvalSymbol(blimp, expr->bind.message, scope, &message)
                    != BLIMP_OK)
            {
                return ErrorFrom(
                    blimp,
                    expr->bind.receiver->range,
                    BLIMP_MUST_BE_SYMBOL,
                    "bind message must evaluate to a symbol"
                );
            }

            return Blimp_BindExpr(blimp, receiver, message, expr->bind.code);
        }

        case EXPR_SEND: {
            Status ret = BLIMP_OK;

            Object *receiver;
            if ((ret = Blimp_Eval(blimp, expr->send.receiver, scope, &receiver))
                    != BLIMP_OK)
            {
                goto err_receiver;
            }

            Object *message;
            if ((ret = Blimp_Eval(blimp, expr->send.message, scope, &message))
                    != BLIMP_OK)
            {
                goto err_message;
            }

            if ((ret = Blimp_Send(blimp, scope, receiver, message, result))
                    != BLIMP_OK)
            {
                goto err_send;
            }

err_send:
            BlimpObject_Release(message);
err_message:
            BlimpObject_Release(receiver);
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

        case EXPR_SEQ:
            TRY(Blimp_Eval(blimp, expr->seq.fst, scope, result));
            BlimpObject_Release(*result);
            return Blimp_Eval(blimp, expr->seq.snd, scope, result);

        default:
            assert(false);
            return ErrorFrom(
                blimp,
                expr->range,
                BLIMP_INVALID_EXPR,
                "internal error: expression is invalid");
    }
}

Status Blimp_EvalSymbol(
    Blimp *blimp,
    const Expr *expr,
    Object *scope,
    const Symbol **sym)
{
    Object *obj;
    TRY(Blimp_Eval(blimp, expr, scope, &obj));

    Status ret = BlimpObject_ParseSymbol(obj, sym);
    BlimpObject_Release(obj);
    return ret;
}
