#include "internal/blimp.h"
#include "internal/expr.h"
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
    memset(&blimp->last_error, 0, sizeof(blimp->last_error));

    if (SymbolTable_Init(blimp, &blimp->symbols) != BLIMP_OK) {
        goto err_symbols;
    }

    if (ObjectPool_Init(blimp, &blimp->objects) != BLIMP_OK) {
        goto err_objects;
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
    ObjectPool_Destroy(blimp, &blimp->objects);
err_objects:
err_symbols:
    free(blimp);
err_malloc:
    return NULL;
}

void Blimp_Delete(Blimp *blimp)
{
    Stack_Destroy(blimp, &blimp->stack);
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
    const Symbol *msg_name,
    Expr *code,
    Value *result)
{
    switch (result->type) {
        case VALUE_OBJECT:
            return BlimpObject_NewBlock(
                blimp,
                scope,
                msg_name,
                code,
                &result->obj);

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
    Value *result,
    const SourceRange *range);

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
            return ReturnBlock(
                blimp, scope, expr->block.msg_name, expr->block.code, result);
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

            if ((ret = Send(
                    blimp,
                    scope,
                    receiver.obj,
                    message.obj,
                    result,
                    &expr->range)) != BLIMP_OK)
            {
                goto err_send;
            }

err_send:
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

        case EXPR_MSG: {
            Object *msg;
            if (expr->msg.index == 0) {
                msg = Stack_CurrentFrame(&blimp->stack)->message;
            } else {
                TRY(BlimpObject_GetMessage(scope, expr->msg.index - 1, &msg));
            }

            BlimpObject_Borrow(msg);
            return ReturnObject(blimp, msg, result);
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
    Object *context,
    Object *receiver,
    Object *message,
    Value *result,
    const SourceRange *range)
{
    Status status = Stack_Push(blimp, &blimp->stack, &(StackFrame){
        .range     = range ? *range : ((SourceRange){{0},{0}}),
        .has_range = range != NULL,
        .message   = message,
    }, 128);

    if (status != BLIMP_OK) {
        return status;
    }

    switch (receiver->type) {
        case OBJ_SYMBOL: {
            // The behavior of a symbol when it receives a message depends on
            // whether the symbol is already in scope or not:
            Object *value;
            if(BlimpObject_Get(context, receiver->symbol, &value) == BLIMP_OK) {
                // If the symbol already has a value in this scope, we simply
                // forward the message on to the value of the symbol.
                status = Send(blimp, context, value, message, result, NULL);
            } else {
                // Otherwise, the message is treated as an initializer for the
                // symbol. We create a new reference object, which can be used
                // to write a value to the symbol by sending a message to the
                // reference, and we send the reference as a message to the
                // message received by the symbol.
                Object *reference;
                if ((status = BlimpObject_NewReference(
                        blimp, context, receiver->symbol, &reference))
                    != BLIMP_OK)
                {
                    break;
                }
                status = Send(blimp, context, message, reference, result, NULL);
                BlimpObject_Release(reference);
            }

            break;
        }

        case OBJ_REFERENCE: {
            // When a reference object (created in the previous case) receives a
            // message, it sets the value of the associated symbol to the
            // message.
            if ((status = BlimpObject_Set(
                    receiver->parent, receiver->symbol, message))
                != BLIMP_OK)
            {
                break;
            }

            // The result is the symbol associated with the reference.
            status = ReturnSymbol(blimp, context, receiver->symbol, result);
            break;
        }

        case OBJ_BLOCK: {
            status = EvalExpr(blimp, receiver->code, receiver, result);
            break;
        }

        case OBJ_EXTENSION: {
            Object *obj;
            if ((status = receiver->ext.method(
                    blimp,
                    context,
                    receiver,
                    message,
                    &obj))
                != BLIMP_OK)
            {
                break;
            }

            status = ReturnObject(blimp, obj, result);
            break;
        }

        default: {
            status = Error(blimp, BLIMP_INVALID_OBJECT_TYPE);
        }
    }

    Stack_Pop(blimp, &blimp->stack);
    return status;
}

Status Blimp_Eval(
    Blimp *blimp,
    const Expr *expr,
    Object *scope,
    Object **obj)
{
    Value v;
    v.type = obj ? VALUE_OBJECT : VALUE_VOID;

    TRY(EvalExpr(blimp, expr, scope, &v));

    if (obj) {
        *obj = v.obj;
    }

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

BlimpStatus Blimp_Send(
    Blimp *blimp,
    Object *scope,
    Object *receiver,
    Object *message,
    Object **result)
{
    Value v = {.type=VALUE_OBJECT};
    TRY(Send(blimp, scope, receiver, message, &v, NULL));
    *result = v.obj;
    return BLIMP_OK;
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
