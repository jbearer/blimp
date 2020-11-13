#include <assert.h>

#include "internal/expr.h"

Status BlimpExpr_NewSymbol(Blimp *blimp, const Symbol *sym, Expr **expr)
{
    *expr = calloc(1, sizeof(Expr));
    if (*expr == NULL) {
        return Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY);
    }

    (*expr)->tag      = EXPR_SYMBOL;
    (*expr)->refcount = 1;
    (*expr)->analysis = NULL;
    (*expr)->next     = NULL;
    (*expr)->symbol   = sym;

    return BLIMP_OK;
}

void Blimp_FreeExpr(Expr *expr)
{
    while (expr) {
        assert(expr->refcount > 0);

        if (--expr->refcount != 0) {
            return;
        }

        if (expr->analysis != NULL) {
            free(expr->analysis);
        }

        // Free subexpressions.
        switch (expr->tag) {
            case EXPR_SYMBOL:
                // Do nothing. Symbols are global, and they don't get cleaned up
                // until the entire bl:mp is destroyed.
                break;
            case EXPR_BLOCK:
                // We don't have to clean up the `msg_name` symbol, for the same
                // reason as above.
                Blimp_FreeExpr(expr->block.code);
                break;
            case EXPR_SEND:
                Blimp_FreeExpr(expr->send.receiver);
                Blimp_FreeExpr(expr->send.message);
                break;
            case EXPR_MSG:
                break;
        }

        Expr *next = expr->next;
        free(expr);
        expr = next;
    }
}

static void DumpSymbol(FILE *file, const Symbol *sym)
{
    // Certain valid bl:mp symbols are given special meaning in the Redex
    // semantics; for example, `obj` is a keyword that indicates a block values.
    // Therefore, symbol literals need to be escaped somehow. We will surround
    // all symbol literals with {} -- neither valid bl:mp characters nor
    // meaningful Redex characters.
    //
    // Redex also assigns _ semantic meaning if it appears anywhere in a Redex
    // term. We need to replace all occurences of _ in this bl:mp symbol with an
    // escape identifier which is not otherwise a valid bl:mp identifier. We
    // will use {-}.
    //
    // Of course, braces are not allowed in Racket symbols either, so we also
    // wrap the whole symbol in ||. Technically we only need to do this if we
    // actually find an underscore in the symbol name, but it's simpler to
    // always do it. This also automatically takes care of characters which are
    // interpreted by Racket but not Redex, such as `.`. It also has the nice
    // side-effect of making it clear in the output what is a symbol.
    fputs("|{", file);
    for (const char *c = sym->name; *c; ++c) {
        if (*c == '_') {
            fputs("{-}", file);
        } else {
            fputc(*c, file);
        }
    }
    fputs("}|", file);
}

static void DumpExpr(FILE *file, const Expr *expr, DeBruijnMap *scopes)
{
    size_t seq_length = 0;
    while (expr) {
        if (expr->next) {
            fputs("(seq ", file);
            ++seq_length;
        }

        switch (expr->tag) {
            case EXPR_SYMBOL:
                DumpSymbol(file, expr->symbol);
                break;
            case EXPR_BLOCK:
                fputs("(block ^", file);
                DumpSymbol(file, expr->block.msg_name);
                fputc(' ', file);

                DBMap_Push(scopes, (void *)expr->block.msg_name);
                DumpExpr(file, expr->block.code, scopes);
                DBMap_Pop(scopes);

                fputc(')', file);
                break;
            case EXPR_SEND:
                fputc('(', file);
                DumpExpr(file, expr->send.receiver, scopes);
                fputc(' ', file);
                DumpExpr(file, expr->send.message, scopes);
                fputc(')', file);
                break;
            case EXPR_MSG:
                fputc('^', file);
                DumpSymbol(file, DBMap_Resolve(scopes, expr->msg.index));
                break;
            default:
                assert(false);
        }

        if (expr->next) {
            fputc(' ', file);
        }

        expr = expr->next;
    }

    for (size_t i = 0; i < seq_length; ++i) {
        fputc(')', file);
    }
}

void Blimp_DumpExpr(Blimp *blimp, FILE *file, const Expr *expr)
{
    DeBruijnMap scopes;
    DBMap_Init(blimp, &scopes);
    DumpExpr(file, expr, &scopes);
    DBMap_Destroy(&scopes);
}

void PrintClosure(FILE *f, const Expr *expr, DeBruijnMap *scopes)
{
    while (expr) {
        switch (expr->tag) {
            case EXPR_SYMBOL:
                fputs(expr->symbol->name, f);
                break;
            case EXPR_BLOCK:
                fputs("{^", f);
                fputs(expr->block.msg_name->name, f);
                fputc(' ', f);

                DBMap_Push(scopes, (void *)expr->block.msg_name);
                PrintClosure(f, expr->block.code, scopes);
                DBMap_Pop(scopes);

                fputc('}', f);
                break;
            case EXPR_SEND:
                fputc('(', f);
                PrintClosure(f, expr->send.receiver, scopes);
                fputs(") (", f);
                PrintClosure(f, expr->send.message, scopes);
                fputc(')', f);
                break;
            case EXPR_MSG: {
                const Symbol *msg_name = DBMap_Resolve(scopes, expr->msg.index);
                fputc('^', f);
                if (msg_name) {
                    fputs(msg_name->name, f);
                } else {
                    fputc('?', f);
                }
                break;
            }
            default:
                assert(false);
        }

        if (expr->next) {
            fputs("; ", f);
        }
        expr = expr->next;
    }
}

void Blimp_PrintExpr(Blimp *blimp, FILE *f, const Expr *expr)
{
    DeBruijnMap scopes;
    DBMap_Init(blimp, &scopes);
    PrintClosure(f, expr, &scopes);
    DBMap_Destroy(&scopes);
}
