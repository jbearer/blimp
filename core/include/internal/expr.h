#ifndef BLIMP_EXPR_H
#define BLIMP_EXPR_H

#include "internal/blimp.h"
#include "internal/symbol.h"

struct BlimpExpr {
    enum {
        EXPR_SYMBOL,
        EXPR_BLOCK,
        EXPR_SEND,
        EXPR_MSG,
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
            const Symbol *msg_name;
                // The name of the free variable in `code` representing a
                // message passed to this object. Uses of the free variable in
                // code are represented as DeBruijn indices resolved at parse
                // time, not as names, so this name is only used for debugging
                // and pretty-printing.
            Expr *code;
        } block;

        struct {
            Expr *receiver;
            Expr *message;
        } send;

        struct {
            size_t index;
                // DeBruijn index of the message being referred to by this
                // expression.
                //
                // `index` refers to the depth in lexical scopes from the object
                // which received the referred-to message, to the reference to
                // the message. For example:
                //
                // {^msg
                //      ^msg
                // }
                //
                // The reference to ^msg in the body of the object occurs in the
                // same lexical scope where ^msg is bound; there are 0 lexical
                // scopes between the two, so the body of this object would be
                // represented by an EXPR_MSG with an index of 0.
                //
                // In another example:
                //
                // {^msg
                //      {
                //          ^msg
                //      }
                // }
                //
                // ^msg in the body of the inner object refers to the message
                // being received by the outer object; there is one lexical
                // scope in between (the anonymous inner scope) so `index` here
                // is 1.
                //
                // The parser ensures that all EXPR_MSG expressions have a valid
                // index; that is, if `index` is `n`, then the expression is a
                // lexical child of at least `n + 1` nested scopes.
        } msg;
    };
};

PRIVATE void PrintClosure(FILE *f, const Expr *expr, DeBruijnMap *scopes);

#endif
