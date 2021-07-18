////////////////////////////////////////////////////////////////////////////////
// Static analysis
//
// This module performs a static analysis pass over expressions, annotating them
// with information that the compiler can use to generate more optimized
// bytecode.
//
// Each expression and sub-expression in a program can have static analysis
// information attached to it. This information can be analyzed in a convenient
// way using the API provided; for example, once an expression has been
// analyzed, the function Expr_IsPure() can be used to determine if it is
// definitely pure (has no side-effects), definitely impure (has side-effects),
// or unknown.
//
// Static analysis information will not be attached to an expression until
// Expr_Analyze() is used to analyze that expression and all of its sub-
// expressions. Therefore, most of the analysis functions will return MAYBE if
// Expr_Analyze() has not been called.
//

#ifndef BLIMP_ANALYZE_H
#define BLIMP_ANALYZE_H

#include "common.h"

typedef enum {
    NO,
    YES,
    MAYBE
} Tristate;

typedef struct {
    // Data about a sequence of expressions (stored in the head of the
    // sequence.)
    const Symbol *sym_value;
    Tristate pure;
    Tristate uses_scope;

    // Data about this expression itself.
    Tristate captures_parents_message;
    Tristate uses_message;
} Analysis;

PRIVATE Status Expr_Analyze(Blimp *blimp, Expr *expr);

PRIVATE Tristate Expr_EvaluatesToSymbol(Expr *expr, const Symbol **sym);
PRIVATE Tristate Expr_IsPure(Expr *expr);
PRIVATE Tristate Block_IsPure(Expr *expr);
PRIVATE Tristate Stmt_IsPure(Expr *stmt);
PRIVATE Tristate Expr_UsesScope(Expr *expr);
PRIVATE Tristate Block_UsesScope(Expr *expr);
PRIVATE Tristate Stmt_UsesScope(Expr *stmt);
PRIVATE Tristate Expr_CapturesParentsMessage(Expr *expr);
PRIVATE Tristate Expr_UsesMessage(Expr *expr);

#endif
