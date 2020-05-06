#include "internal/blimp.h"
#include "internal/expr.h"
#include "internal/symbol.h"

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

    if (ObjectPool_Init(blimp, &blimp->objects) != BLIMP_OK) {
        goto err_objects;
    }

    return blimp;

err_objects:
    SymbolTable_Destroy(&blimp->symbols);
err_symbols:
    free(blimp);
err_malloc:
    return NULL;
}

void Blimp_Delete(Blimp *blimp)
{
    ObjectPool_Destroy(blimp, &blimp->objects);
    SymbolTable_Destroy(&blimp->symbols);
    free(blimp);
}

Status Blimp_GetSymbol(Blimp *blimp, const char *name, const Symbol **symbol)
{
    return SymbolTable_GetSymbol(&blimp->symbols, name, symbol);
}

Status Blimp_Eval(
    Blimp *blimp, const Expr *expr, Object *scope, Object **result)
{
    switch (expr->tag) {
        case EXPR_SYMBOL:
            return BlimpObject_NewSymbol(blimp, scope, expr->symbol, result);

        case EXPR_BLOCK: {
            Object *tag_object;
            TRY(Blimp_Eval(blimp, expr->block.tag, scope, &tag_object));

            const Symbol *tag;
            if (BlimpObject_ParseSymbol(tag_object, &tag) != BLIMP_OK) {
                return ErrorFrom(
                    blimp,
                    expr->block.tag->range,
                    BLIMP_MUST_BE_SYMBOL,
                    "object tag must evaluate to a symbol");
            }

            return BlimpObject_NewBlock(
                blimp, scope, tag, expr->block.code, result);
        }

        case EXPR_SEQ:
            TRY(Blimp_Eval(blimp, expr->seq.fst, scope, result));
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
