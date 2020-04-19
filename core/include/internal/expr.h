#ifndef BLIMP_EXPR_H
#define BLIMP_EXPR_H

#include "internal/blimp.h"

struct BlimpSymbol {
    const char *name;
    size_t hash;
    size_t length;
};

struct BlimpExpr {
    enum {
        EXPR_SYMBOL,
        EXPR_BLOCK,
        EXPR_SEND,
        EXPR_BIND,
        EXPR_SEQ,
    } tag;

    SourceRange range;

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

        struct {
            Expr *fst;
            Expr *snd;
        } seq;
    };
};

#endif
