#include <assert.h>

#include "internal/expr.h"

void Blimp_DumpExpr(FILE *file, const Expr *expr)
{
    switch (expr->tag) {
        case EXPR_SYMBOL:
            // Certain valid bl:mp symbols are given special meaning in the
            // Redex semantics; for example, `symbol` and `block` are keywords
            // that indicate symbol and block values. Therefore, symbol literals
            // need to be escaped somehow. We will surround all symbol literals
            // with {} -- neither valid bl:mp characters nor meaningful Redex
            // characters.
            //
            // Redex also assigns _ semantic meaning if it appears anywhere in a
            // Redex term. We need to replace all occurences of _ in this bl:mp
            // symbol with an escape identifier which is not otherwise a valid
            // bl:mp identifier. We will use {-}.
            //
            // Of course, braces are not allowed in Racket symbols either, so we
            // also wrap the whole symbol in ||. Technically we only need to do
            // this if we actually find an underscore in the symbol name, but
            // it's simpler to always do it. This also automatically takes care
            // of characters which are interpreted by Racket but not Redex, such
            // as `.`. It also has the nice side-effect of making it clear in
            // the output what is a symbol.
            fputs("|{", file);
            for (const char *c = expr->symbol->name; *c; ++c) {
                if (*c == '_') {
                    fputs("{-}", file);
                } else {
                    fputc(*c, file);
                }
            }
            fputs("}|", file);

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
