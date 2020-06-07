#ifndef BLIMP_EXPR_H
#define BLIMP_EXPR_H

#include "internal/blimp.h"
#include "internal/symbol.h"

struct BlimpExpr {
    enum {
        EXPR_SYMBOL,
        EXPR_BLOCK,
        EXPR_SEND,
        EXPR_BIND,
    } tag;

    size_t refcount;
    SourceRange range;

    Expr *next;
        // Next expression in a sequene of expressions starting with this one.
        // Sequences (the ; operator) are represented as a list (rather than a
        // recursive algebraic data type like the rest of the expressions) to
        // make them easier to process in an iterative fashion.
        //
        // Processing sequence expressions recursively is problematic because
        // the length of a bl:mp program is primarily made up of a single long
        // sequence, so processing that sequence recursively would require stack
        // depth proportional to the length of the input program. For other
        // kinds of expressions, the depth of the AST is small and bounded in
        // reasonable input programs.

    union {
        const Symbol *symbol;

        struct {
            Expr *tag;
            Expr *code;
        } block;

        struct {
            Expr *receiver;
            Expr *message;
        } send;

        struct {
            Expr *receiver;
            Expr *message;
            Expr *code;
        } bind;
    };
};

#endif
