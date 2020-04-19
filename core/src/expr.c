#include <assert.h>

#include "internal/expr.h"

void Blimp_DumpExpr(FILE *file, const Expr *expr)
{
    switch (expr->tag) {
        case EXPR_SYMBOL:
            fprintf(file, "%s", expr->symbol->name);
            break;
        case EXPR_BLOCK:
            fputs("(block ", file);
            Blimp_DumpExpr(file, expr->block.tag);
            fputc(' ', file);
            Blimp_DumpExpr(file, expr->block.code);
            fputc(')', file);
            break;
        case EXPR_SEND:
            fputc('(', file);
            Blimp_DumpExpr(file, expr->send.receiver);
            fputc(' ', file);
            Blimp_DumpExpr(file, expr->send.message);
            fputc(')', file);
            break;
        case EXPR_BIND:
            fputs("(bind ", file);
            Blimp_DumpExpr(file, expr->bind.receiver);
            fputc(' ', file);
            Blimp_DumpExpr(file, expr->bind.message);
            fputc(' ', file);
            Blimp_DumpExpr(file, expr->bind.code);
            fputc(')', file);
            break;
        case EXPR_SEQ:
            fputs("(seq ", file);
            Blimp_DumpExpr(file, expr->seq.fst);
            fputc(' ', file);
            Blimp_DumpExpr(file, expr->seq.snd);
            fputc(')', file);
            break;
        default:
            assert(false);
    }
}
