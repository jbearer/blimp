#ifndef BLIMP_GRAMMAR_H
#define BLIMP_GRAMMAR_H

#include "internal/parse.h"

////////////////////////////////////////////////////////////////////////////////
// the built-in grammar
//
// The bl:mp grammar is fundamentally simple. We have five kinds of expressions:
//  1. Sequences        A; B
//  2. Sends            A B
//  3. Blocks           {^msg A}
//  4. Symbols          sym
//  5. Message names    ^msg
//
// These are conceptually divided into expressions, statements, and terms to
// indicate increasing precedence:
//
//  Expr -> Stmt ; Expr | Stmt
//  Stmt -> Stmt Expr | Term
//  Term -> {^msg Expr}
//        | sym
//        | ^msg
//        | ( Expr )
//
// Unfortunately, the ^msg part of the block syntax is optional, and this
// complicates everything. Consider {^msg foo}. This could be parsed as a block
// with a message name specified, whose body expression is a single symbol
// `foo`. Or it could be parsed as a block with no message name whose body
// expression is a send `^msg foo` (where `^msg` is acting not as the message
// name for the block but as a term on its own). The rule is that the former
// takes precedence. In order to encode this rule, we need to factorize the
// grammar so we can express that a block either begins with a message name
// follwed by an expression, or without a message name and an expression _that
// does not start with a message name_:
//
//  Expr      -> Stmt ; Expr
//             | Stmt
//  ExprNoMsg -> StmtNoMsg ; Expr
//             | StmtNoMsg
//  Stmt      -> Stmt Expr
//             | Term
//  StmtNoMsg -> StmtNoMsg Expr
//             | TermNoMsg
//  Term      -> TermNoMsg | ^msg
//  TermNoMsg -> {^msg Expr}
//             | {ExprNoMsg}
//             | sym
//             | ( Expr )
//
// Here, the "NoMsg" non-terminals represent everything from the language of the
// corresponding non-terminal except sentences that start with a ^msg token.
//
// Finally, there is one more factorization we need to do. We want the parser to
// update itself with new productions added by macros at least as often as in
// between top-level statements; that is, after parsing `Stmt ;` or `StmtNoMsg
// ;`. Unfortunately, at such a point in parsing, the presence of `;` (which,
// being a terminal, has higher precedence than any non-terminal) in the output
// precludes us from being able to add new productions that start with a
// non-terminal (even if that non-terminal is relatively high precedence, like
// Term). To fix this, we force the parser to reduce `;` into a low-precedence
// non-terminal as soon as it sees it, by adding the rule `Semi -> ;`, where
// `Semi` has very low precedence. We then replace the two productions that used
// the `;` terminal with:
//
//  Expr      -> Stmt Semi Expr
//  ExprNoMsg -> StmtNoMsg Semi Expr
//
// Now, after parsing a top-level statement, the output consists only of two
// non-terminals `Stmt Semi`, both of which have lower precedence than most
// non-terminals that we would find in a newly added rule.

PRIVATE Status DefaultGrammar(Blimp *blimp, Grammar *grammar);
PRIVATE Status ParseTreeToExpr(Blimp *blimp, ParseTree *tree, Expr **expr);
PRIVATE Status DefineMacro(
    Blimp *blimp, Object *production, Object *handler, const Symbol **nt_sym);

#endif

