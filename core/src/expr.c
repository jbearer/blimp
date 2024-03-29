#include <assert.h>

#include "internal/expr.h"

static Status Expr_New(Blimp *blimp, ExprType type, Expr **expr)
{
    TRY(Calloc(blimp, 1, sizeof(Expr), expr));

    (*expr)->blimp = blimp;
    (*expr)->tag = type;
    (*expr)->refcount = 1;
    (*expr)->next = NULL;
    (*expr)->last = *expr;
    return BLIMP_OK;
}

Status BlimpExpr_NewSymbol(Blimp *blimp, const Symbol *sym, Expr **expr)
{
    return BlimpExpr_NewObject(blimp, (Object *)sym, expr);
}

Status BlimpExpr_NewBlock(
    Blimp *blimp, const Symbol *msg_name, Expr *code, Expr **expr)
{
    TRY(Expr_New(blimp, EXPR_BLOCK, expr));
    (*expr)->block.msg_name = msg_name;
    (*expr)->block.code = BlimpExpr_Borrow(code);

    if (msg_name == NULL) {
        // If no message name was given explicitly, generate a unique one.
        char buffer[32];
        snprintf(buffer, sizeof(buffer), "^%zu^", blimp->counter++);
        TRY(Blimp_GetSymbol(blimp, buffer, &(*expr)->block.msg_name));
    }

    return BLIMP_OK;
}

Status BlimpExpr_NewSend(
    Blimp *blimp, Expr *receiver, Expr *message, Expr **expr)
{
    TRY(Expr_New(blimp, EXPR_SEND, expr));
    (*expr)->send.receiver = BlimpExpr_Borrow(receiver);
    (*expr)->send.message  = BlimpExpr_Borrow(message);
    return BLIMP_OK;
}

Status BlimpExpr_NewMacro(
    Blimp *blimp, Expr *production, Expr *handler, Expr **expr)
{
    TRY(Expr_New(blimp, EXPR_MACRO, expr));
    (*expr)->macro.production = BlimpExpr_Borrow(production);
    (*expr)->macro.handler    = BlimpExpr_Borrow(handler);
    return BLIMP_OK;
}

Status BlimpExpr_NewMsgName(Blimp *blimp, const Symbol *name, Expr **expr)
{
    TRY(Expr_New(blimp, EXPR_MSG_NAME, expr));
    (*expr)->msg_name = name;
    return BLIMP_OK;
}

Status BlimpExpr_NewMsgIndex(Blimp *blimp, size_t index, Expr **expr)
{
    TRY(Expr_New(blimp, EXPR_MSG, expr));
    (*expr)->msg.index = index;
    return BLIMP_OK;
}

Status BlimpExpr_NewObject(Blimp *blimp, Object *object, Expr **expr)
{
    TRY(Expr_New(blimp, EXPR_OBJECT, expr));
    (*expr)->object = BlimpObject_Borrow(object);
    return BLIMP_OK;
}

Status BlimpExpr_ParseSymbol(Expr *expr, const Symbol **sym)
{
    if (expr->tag != EXPR_OBJECT) {
        return Blimp_Error(expr->blimp, BLIMP_MUST_BE_SYMBOL);
    }

    return BlimpObject_ParseSymbol(expr->object, sym);
}

static Status ResolveExpr(Blimp *blimp, Expr *expr, DeBruijnMap *scopes);

static Status ResolveStmt(Blimp *blimp, Expr *stmt, DeBruijnMap *scopes)
{
    switch (stmt->tag) {
        case EXPR_OBJECT:
            return BLIMP_OK;
        case EXPR_MSG:
            // Validate the message index, since downstream passes assume that
            // all message expressions refer to a parent that actually exists.
            if (stmt->msg.index >= DBMap_Size(scopes)) {
                return ErrorFrom(blimp, stmt->range, BLIMP_INVALID_MESSAGE_NAME,
                    "expression refers to a message which is not in scope");
            }
            return BLIMP_OK;
        case EXPR_SEND:
            // Recurse into sub-expressions.
            TRY(ResolveExpr(blimp, stmt->send.receiver, scopes));
            TRY(ResolveExpr(blimp, stmt->send.message, scopes));
            return BLIMP_OK;
        case EXPR_MACRO:
            // Recurse into sub-expressions.
            TRY(ResolveExpr(blimp, stmt->macro.production, scopes));
            TRY(ResolveExpr(blimp, stmt->macro.handler, scopes));
            return BLIMP_OK;
        case EXPR_BLOCK:
            DBMap_Push(scopes, (void *)stmt->block.msg_name);
                // Register the message name associated with this block so we
                // can look it up if we encounter any EXPR_MSG_NAME in the body.
            TRY(ResolveExpr(blimp, stmt->block.code, scopes));
                // Recurse into the subexpression.
            DBMap_Pop(scopes);
            return BLIMP_OK;
        case EXPR_MSG_NAME: {
            const Symbol *sym = stmt->msg_name;
            stmt->tag = EXPR_MSG;
            if (DBMap_Index(
                    scopes,
                    (void *)sym,
                    (void *)BlimpSymbol_Eq,
                    &stmt->msg.index)
                != BLIMP_OK)
            {
                return ErrorFrom(blimp, stmt->range, BLIMP_INVALID_MESSAGE_NAME,
                    "no message named ^%s in scope", sym->name);
            }
            return BLIMP_OK;
        }
        default:
            assert(false);
            return Error(blimp, BLIMP_INVALID_EXPR);
    }
}

static Status ResolveExpr(Blimp *blimp, Expr *expr, DeBruijnMap *scopes)
{
    for (Expr *stmt = expr; stmt != NULL; stmt = stmt->next) {
        TRY(ResolveStmt(blimp, stmt, scopes));
    }

    return BLIMP_OK;
}

Status BlimpExpr_Resolve(Blimp *blimp, Expr *expr)
{
    DeBruijnMap scopes;
    DBMap_Init(blimp, &scopes);

    if (ResolveExpr(blimp, expr, &scopes) != BLIMP_OK) {
        DBMap_Destroy(&scopes);
        return Reraise(blimp);
    }

    DBMap_Destroy(&scopes);
    return BLIMP_OK;
}

bool BlimpExpr_HasSourceRange(const Expr *expr)
{
    return expr->range.start.file != NULL;
}

void BlimpExpr_SetSourceRange(Expr *expr, const SourceRange *range)
{
    expr->range = *range;
}

Expr *BlimpExpr_Borrow(Expr *expr)
{
    assert(expr->refcount > 0);
    ++expr->refcount;
    return expr;
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
            case EXPR_OBJECT:
                BlimpObject_Release(expr->object);
                break;
            case EXPR_BLOCK:
                // We don't have to clean up the `msg_name` symbol. Symbols are
                // global, and they don't get cleaned up until the entire bl:mp
                // is destroyed.
                Blimp_FreeExpr(expr->block.code);
                break;
            case EXPR_SEND:
                Blimp_FreeExpr(expr->send.receiver);
                Blimp_FreeExpr(expr->send.message);
                break;
            case EXPR_MACRO:
                Blimp_FreeExpr(expr->macro.production);
                Blimp_FreeExpr(expr->macro.handler);
                break;
            case EXPR_MSG:
                break;
            case EXPR_MSG_NAME:
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

void DumpClosure(FILE *file, const Expr *expr, DeBruijnMap *scopes)
{
    size_t seq_length = 0;
    while (expr) {
        if (expr->next) {
            fputs("(seq ", file);
            ++seq_length;
        }

        switch (expr->tag) {
            case EXPR_OBJECT:
                fputs("(value ", file);
                BlimpObject_Dump(file, expr->object);
                fputc(')', file);
                break;
            case EXPR_BLOCK:
                fputs("(block ^", file);
                DumpSymbol(file, expr->block.msg_name);
                fputc(' ', file);

                DBMap_Push(scopes, (void *)expr->block.msg_name);
                DumpClosure(file, expr->block.code, scopes);
                DBMap_Pop(scopes);

                fputc(')', file);
                break;
            case EXPR_SEND:
                fputc('(', file);
                DumpClosure(file, expr->send.receiver, scopes);
                fputc(' ', file);
                DumpClosure(file, expr->send.message, scopes);
                fputc(')', file);
                break;
            case EXPR_MACRO:
                fputs("(macro ", file);
                DumpClosure(file, expr->macro.production, scopes);
                fputc(' ', file);
                DumpClosure(file, expr->macro.handler, scopes);
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
    DumpClosure(file, expr, &scopes);
    DBMap_Destroy(&scopes);
}

void PrintClosure(FILE *f, const Expr *expr, DeBruijnMap *scopes)
{
    while (expr) {
        switch (expr->tag) {
            case EXPR_OBJECT:
                BlimpObject_Print(f, expr->object);
                break;
            case EXPR_BLOCK:
                if (Expr_MessageCaptured((Expr *)expr) != NO) {
                    fputs("{^", f);
                    fputs(expr->block.msg_name->name, f);
                    fputc(' ', f);
                } else {
                    fputc('{', f);
                }

                DBMap_Push(scopes, (void *)expr->block.msg_name);
                PrintClosure(f, expr->block.code, scopes);
                DBMap_Pop(scopes);

                fputc('}', f);
                break;
            case EXPR_SEND:
                fputc('(', f);
                PrintClosure(f, expr->send.receiver, scopes);
                fputc(' ', f);
                PrintClosure(f, expr->send.message, scopes);
                fputc(')', f);
                break;
            case EXPR_MACRO:
                fputs("(\\> ", f);
                PrintClosure(f, expr->macro.production, scopes);
                fputc(' ', f);
                PrintClosure(f, expr->macro.handler, scopes);
                fputc(')', f);
                break;
            case EXPR_MSG: {
                fputc('^', f);
                if (expr->msg.index != 0) {
                    const Symbol *msg_name = DBMap_Resolve(
                        scopes, expr->msg.index);
                    if (msg_name) {
                        fputs(msg_name->name, f);
                    } else {
                        fputc('?', f);
                    }
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
