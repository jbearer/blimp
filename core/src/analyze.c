#include "internal/analyze.h"
#include "internal/debruijn.h"
#include "internal/expr.h"

static Status NewAnalysis(Blimp *blimp, Analysis **analysis)
{
    TRY(Malloc(blimp, sizeof(Analysis), analysis));

    // Initialize everything to safe, uncertain values.
    (*analysis)->sym_value = NULL;
    (*analysis)->pure = MAYBE;
    (*analysis)->uses_scope = MAYBE;

    (*analysis)->captures_parents_message = NO;
        // This property can be decided statically, so its value should never be
        // MAYBE. We initialize it to NO, and change it to YES if we ever see
        // the message get captured.
    (*analysis)->uses_message = NO;
        // This property can be decided statically, so its value should never be
        // MAYBE. We initialize it to NO, and change it to YES if we ever see
        // the message get used.

    return BLIMP_OK;
}

static inline Tristate Tristate_And(Tristate t1, Tristate t2)
{
    switch (t1) {
        case YES:
            return t2;
        case NO:
            return NO;
        default:
            return t2 == NO ? NO : MAYBE;
    }
}

static inline Tristate Tristate_Or(Tristate t1, Tristate t2)
{
    switch (t1) {
        case YES:
            return YES;
        case NO:
            return t2;
        default:
            return t2 == YES ? YES : MAYBE;
    }
}

static Status AnalyzeExpr(Blimp *blimp, Expr *expr, DeBruijnMap *scopes);
static Status AnalyzeStmt(Blimp *blimp, Expr *expr, DeBruijnMap *scopes);

// Analyze `expr`. After this function succeeds, every subexpression of `expr`
// which is either the first statement in a sequence, or a block statement, will
// have analsyis information attached summarizing properties of the entire
// sequence, or the entire subexpression tree, respectively.
static Status AnalyzeExpr(Blimp *blimp, Expr *expr, DeBruijnMap *scopes)
{
    const Symbol *sym_value = NULL;
    Tristate pure = YES;
    Tristate uses_scope = NO;

    for (Expr *stmt = expr; stmt != NULL; stmt = stmt->next) {
        TRY(AnalyzeStmt(blimp, stmt, scopes));
            // Analyze subexpressions.

        if (stmt->next == NULL) {
            Expr_EvaluatesToSymbol(stmt, &sym_value);
                // If this is the last statement in the sequence, then the value
                // of `expr` is the value if this statement. Check if that value
                // is a symbol.
        }

        pure = Tristate_And(pure, Stmt_IsPure(stmt));
            // A sequence is pure if all of its statements are pure.
        uses_scope = Tristate_Or(uses_scope, Stmt_UsesScope(stmt));
            // A sequence uses its scope if any of its statements do.
    }

    // If `expr` is the head of a sequence, attach sequence analysis.
    if (expr->next != NULL) {
        if (expr->analysis == NULL) {
            TRY(NewAnalysis(blimp, &expr->analysis));
        }

        expr->analysis->sym_value = sym_value;
        expr->analysis->pure = pure;
        expr->analysis->uses_scope = uses_scope;
    }

    return BLIMP_OK;
}

static Status AnalyzeStmt(Blimp *blimp, Expr *expr, DeBruijnMap *scopes)
{
    switch (expr->tag) {
        case EXPR_SYMBOL:
            return BLIMP_OK;

        case EXPR_MSG: {
            // Find the block whose message is being referenced and record that
            // it uses its message.
            Analysis *owner = DBMap_Resolve(scopes, expr->msg.index);
            owner->uses_message = YES;

            if (0 < expr->msg.index && expr->msg.index <= DBMap_Size(scopes)) {
                // If the index of this message refers to the message of a block
                // object which is an ancestor of the one currently being
                // analyzed, then the current block captures the message from
                // its ancestor block.
                Analysis *capturing_child = DBMap_Resolve(
                    scopes, expr->msg.index - 1);
                capturing_child->captures_parents_message = YES;
            }
            return BLIMP_OK;
        }

        case EXPR_SEND:
            TRY(AnalyzeExpr(blimp, expr->send.receiver, scopes));
            TRY(AnalyzeExpr(blimp, expr->send.message, scopes));
            return BLIMP_OK;

        case EXPR_MACRO:
            TRY(AnalyzeExpr(blimp, expr->macro.production, scopes));
            TRY(AnalyzeExpr(blimp, expr->macro.handler, scopes));
            return BLIMP_OK;

        case EXPR_BLOCK:
            if (expr->analysis == NULL) {
                TRY(NewAnalysis(blimp, &expr->analysis));
            }

            // Push this expression onto the stack, so insights gained from
            // analyzing its subexpression can be attached to the summary
            // analysis for the entire block.
            TRY(DBMap_Push(scopes, expr->analysis));
            if (AnalyzeExpr(blimp, expr->block.code, scopes) != BLIMP_OK) {
                DBMap_Pop(scopes);
                return Reraise(blimp);
            }
            DBMap_Pop(scopes);

            return BLIMP_OK;

        default:
            return Error(blimp, BLIMP_INVALID_EXPR);
    }
}

Status Expr_Analyze(Blimp *blimp, Expr *expr)
{
    DeBruijnMap scopes;
    DBMap_Init(blimp, &scopes);

    Status status = AnalyzeExpr(blimp, expr, &scopes);

    DBMap_Destroy(&scopes);
    return status;
}

Tristate Expr_EvaluatesToSymbol(Expr *expr, const Symbol **sym)
{
    if (expr->next && expr->analysis) {
        // If a sequence evaluates to a symbol, we have stored that symbol in
        // the analysis for the sequence.
        if (expr->analysis->sym_value) {
            if (sym != NULL) {
                *sym = expr->analysis->sym_value;
            }
            return YES;
        } else {
            return MAYBE;
        }
    } else if (expr->next == NULL) {
        // A single statement evaluates to a symbol if it is a symbol literal or
        // a macro expression.
        if (expr->tag == EXPR_SYMBOL) {
            if (sym != NULL) {
                *sym = expr->symbol;
            }
            return YES;
        } else if (expr->tag == EXPR_MACRO) {
            Expr *production = expr->macro.production->last;
            if (production->tag == EXPR_BLOCK && sym != NULL) {
                // The result of the macro definition is the result of sending a
                // parse tree visitor to the production object. In the common
                // case, the production will be a block which simply returns a
                // symbol literal. In this case, we can easily say exactly what
                // symbol the macro returns.
                //
                // Note that we could be more general here if we had an analysis
                // function like
                //  Send_EvaluatesToSymbol(Expr *receiver, const Symbol **sym)
                // But for now, this special case will catch most macro
                // definitions.
                *sym = production->block.code->analysis->sym_value;
            }
            return YES;
        } else {
            return MAYBE;
        }
    } else {
        return MAYBE;
    }
}

Tristate Expr_IsPure(Expr *expr)
{
    if (expr->next && expr->analysis) {
        // If a sequence is pure, we have stored that information in the
        // sequence analysis.
        return expr->analysis->pure;
    } else if (expr->next == NULL) {
        return Stmt_IsPure(expr);
    } else {
        return MAYBE;
    }
}

Tristate Block_IsPure(Expr *expr)
{
    assert(expr->tag == EXPR_BLOCK);
    return Expr_IsPure(expr->block.code);
}

Tristate Stmt_IsPure(Expr *stmt)
{
    switch (stmt->tag) {
        case EXPR_SYMBOL:
        case EXPR_BLOCK:
        case EXPR_MSG:
            return YES;
        default:
            return MAYBE;
    }
}

Tristate Expr_UsesScope(Expr *expr)
{
    if (expr->next && expr->analysis) {
        // If a sequence uses its scope, we have stored that information in the
        // sequence analysis.
        return expr->analysis->uses_scope;
    } else if (expr->next == NULL) {
        return Stmt_UsesScope(expr);
    } else {
        return MAYBE;
    }
}

Tristate Block_UsesScope(Expr *expr)
{
    assert(expr->tag == EXPR_BLOCK);
    return Expr_UsesScope(expr->block.code);
}

Tristate Stmt_UsesScope(Expr *stmt)
{
    switch (stmt->tag) {
        case EXPR_SEND:
            if (Expr_EvaluatesToSymbol(stmt->send.receiver, NULL) == NO) {
                return NO;
            } else {
                return MAYBE;
            }
        default:
            return NO;
    }
}

Tristate Expr_CapturesParentsMessage(Expr *expr)
{
    if (expr->analysis == NULL) {
        return MAYBE;
    } else {
        return expr->analysis->captures_parents_message;
    }
}

Tristate Expr_UsesMessage(Expr *expr)
{
    if (expr->analysis == NULL) {
        return MAYBE;
    } else {
        return expr->analysis->uses_message;
    }
}
