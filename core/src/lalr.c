////////////////////////////////////////////////////////////////////////////////
// LALR(1) Parser
//
// This file implements a generic, extensible LALR(1) parser generator. The
// algorithm takes a Grammar as input and generates an LALR(1) state machine
// which can be used to parse a sequence of tokens provided by a lexer. The
// comments in this file will assume a basic familiarity with the terminology
// and algorithms for LALR parsing. An excellent overview by Stephen Jackson
// can be found here: https://web.cs.dal.ca/~sjackson/lalr1.html.
//
// The parser implemented here goes beyond the canonical LALR(1) parser in that
// it is capable of parsing input which modifies the grammar, and to continue
// parsing the remaining input using the modified grammar. This allows it to
// parse languages with parse-time-dynamic syntax (e.g. macros).
//
////////////////////////////////////////////////////////////////////////////////
// Grammar
//
// The input to the parser generator, and the object that user's of the parser
// generator have the most interaction with, is a Grammar. The Grammar type
// represents a context-free grammar, but the parser generator will check that
// the given grammar is actually LALR(1) which is a subset of CFG. The grammar
// is given by a sequence of "productions" of the form
//
//      NT -> sym1 sym2 ...
//
// The left-hand side of a production is a "non-terminal", or a symbol which
// stands for sub-trees of a parse tree. The right-hand side is a sequence of
// symbols which are each either non-terminals themselves, or terminals
// (tokens). A production is a rule which defines one way the left-hand non-
// terminal can be parsed. For example, the rule above could be read as, "to
// parse an NT sub-tree, first parse sym1, sym2, and so on, and then reduce
// those sub-trees into an NT node". That would yield a parse tree like
//
//                                  NT                                        //
//                               /   |  \                                     //
//                            sym1 sym2  ...                                  //
//
// There can be multiple rules for the same non-terminal, for example, the rules
//      X -> x y
//      X -> z
// indicate that there are two valid sub-trees that can be rooted at an X node
// in a parse tree:
//
//                  X                              X
//                /   \                            |
//               x     y                           z
//
// Terminals in this file are represented simply as token types which can be
// produced by a lexer. Non-terminals are represented as integers, and the
// convention is to think of the number representing a non-terminal as its
// precedence. For example, take a simple expression grammar:
//
//      Expr -> Expr + Term
//      Term -> Term * Factor
//      Factor -> number
//
// For this grammar, we would represent the non-terminal Expr as 0, Term as 1,
// and Factor as 2, to indicate that Expr's productions have the lowest
// precedence, followed by Term, and then Factor has the highest precedence.
//
// The convention that non-terminals are represented by their precedence is
// actually enforced by the parser generator: Any production for a non-terminal
// N must not start with a non-terminal less than N. Due to this constraint, the
// languages parsable by this algorithm are actually a proper subset of LALR(1),
// but, as we will see, this "well-behaved precedence" property is critical to
// the parser's dynamic grammar-changing capabilities.
//
////////////////////////////////////////////////////////////////////////////////
// LALR(1) Parsing
//
// Now that we know how grammars are represented, we'll have a high-level
// overview of the algorithm used to transform a grammar into a parser. The goal
// of the algorithm is to produce a parse table of this form:
//
//            |         Actions         |           Gotos           |
//      State | t1   t2    ...          | N1   N2    ...            |
//      ------|-------------------------|---------------------------|
//         0  | r0   s1                 |      0                    |
//         1  |      acc                |                           |
//
// The rows of the table correspond to states in the LALR(1) state machine for
// the input grammar.
//
// In each row, there is an "action" column for each terminal
// in the grammar, which tells the parser what to do when that terminal is the
// next token of input. There are 3 kinds of action:
//      rN: Reduce using the production labelled N (in this implementation, the
//          production labels are simply pointers to the corresponding
//          Production datat structures).
//      sN: Shift the token from the input to the stack of output, and go to
//          state N.
//      acc: Accept the input.
// Cells in a row which do not have an action are error cases. If we end up
// there, it means we have seen some unexpected input, and we should report a
// parse error to the caller.
//
// In each row, there is a "goto" column for each non-terminal in th grammar.
// These tell the parser which state to transition to after performing a
// reduction which produced that non-terminal. Unlike the actions, cells in the
// gotos section which are empty represent unreachable state transitions. If we
// end up there, it is not a parse error, but an internal consistency error;
// that is, a bug.
//
// The algorithm to construct this parse table from a grammar is laid out in
// great detail by Jackson. It consists of 3 phases, each producing an
// intermediate output, followed by a summary phase which produces the final
// table.
//
////////////////////////////////////////////////////////////////////////////////
// LALR(1) Parsing, Phase I: The State Machine
//
// In this phase, we construct the states of the LALR(1) automaton for the input
// grammar, as well as the transitions between them. Each state represents a
// possible partial input to the parser, and the transitions indicate how the
// state changes based on the next token of input.
//
// The building block we will use to construct the state machine is an "item".
// An item is a production rule applied to a particular input, written like so:
//      N -> sym1 ⋅ sym2 ...
// The ⋅ represents the position of the input; it comes before the next token to
// be consumed. For each production rule with `n` symbols, we get `n + 1` items
// for each of the possible positions where we could put the ⋅ (between each
// pair of symbols, and at the start and end of the production).
//
// Now, it is not always the case that given a partial input, we can tell
// exactly which item we are parsing. Consider the following simple grammar:
//      A -> x y
//      B -> x z
// Given an input `x ...`, we could be parsing the item `A -> x⋅y` or
// `B -> x⋅z`. Our decision will depend on the next token of input, but since we
// only have one token of lookahead, we can't make the decision yet based on
// seeing `x`. Therefore, each state of the automaton must represent all the
// possible items that we could be parsing based on the input we have seen so
// far. In fact, Jackson even refers to states as "item sets". The state
// described above could be represented like so:
//
//              ------------
//             | A -> x ⋅ y |
//             | B -> x ⋅ z |
//              ------------
//
// The set of items in a state that are all consistent with the same partial
// input is called the "kernel" of the state, and it uniquely determines it.
// That is, states with the same kernel are the same state. However, the kernel
// is not the only information we want to record about a state; for each state,
// there is another set of items that we care about called the "closure".
//
// Consider the state whose kernel consists of the single item `A -> a⋅B` (where
// B is a non-terminal). We want to be able to look at a state and see what
// sorts of input we might expect to encounter next. Here, the position of the ⋅
// before the non-terminal B is telling us that we expect a token or sequence of
// tokens which can be reduced into a B parse tree using some productions. But
// exactly what tokens we expect to see depends on the production rules for B.
// Say:
//      B -> x
//      B -> y
// Since we are expecting a B, and we can reduce a B from either the terminal
// `x` or the terminal `y`, we can expect to see an `x` or a `y` next. To
// indicate this, we add the productions for `B` (with the ⋅ in front) to the
// closure of the state, to get
//           ----------
//          | A -> a⋅B |
//          |----------|
//          | B -> ⋅x  |
//          | B -> ⋅y  |
//           ----------
// In general, to construct the closure of a state, we add the productions for
// each non-terminal which follows a ⋅ in one of the items already in the state.
// We may have to do this iteratively, if we add a new item where the ⋅ comes
// before yet another non-terminal.
//
// Now that we know how to construct states, with their kernels and closures, we
// need to compute the transitions between them to arrive at a parser automaton.
// We begin with a starting state for each non-terminal N (other than the start
// symbol S), whose kernels each contain the single item `S -> ⋅N`. Building all
// of these starting states into the same automaton allows a single parse table
// to parse any desired non-terminal just by choosing the appropriate starting
// state. This is especially important for bl:mp, where macro expansions specify
// a particular non-terminal by which they should be parsed. Since the same
// parse table works for all such parse operations, the table (which is
// expensive to compute) can be effectively cached and reused for all subsequent
// parses with the same grammar.
//
// Now, for each of our starting kernels, we construct the closure of the
// starting state by the process described above. Perhaps it looks something
// like this:
//
//           -----------
//          | S -> ⋅A   |
//          |-----------|
//          | A -> ⋅x B |
//          | A -> ⋅A y |
//           -----------
//
// For each symbol that follows a ⋅ in any item in the state (in this case, `x`
// and `A`) create a new kernel by taking all of the items from the state which
// expect that symbol and advancing the ⋅ past the expected symbol. For example,
// if we see an `A` while in this state, we get two new items: `S -> A⋅` and
// `A -> A⋅y`. These form the kernel of the next state. If there is already a
// state with that kernel, just add a transition on `A` to that state.
// Otherwise, create a new state and compute its closure. Repeat this process
// for all newly added states, until no new states are added.
//
// In this implementation, we take a small shortcut during this phase: instead
// of computing simple transitions between states and then later translating
// those transitions to shift actions and gotos (as Jackson does) we represent
// the transitions in the state machine itself as either shift actions (for
// transitions on terminals) or gotos (for transitions on non-terminals). So, at
// the end of this phase we actually end up with a partially filled out parse
// table. All of the shifts and gotos are there; all that's missing are the
// "accept" and "reduce" actions.
//
////////////////////////////////////////////////////////////////////////////////
// LALR(1) Parsing, Phase II: The Extended Grammar
//
// The extended grammar augments productions from the input grammar with
// information about the state transitions which can be caused by each symbol in
// the production, and by the non-terminal produced. Extended grammar
// productions are written like so:
//      ₒN₁ -> ₒx₃ ₃A₂
// This extended production augments an input production `N -> x A` with the
// information that
//  * If we start in state 0, an N takes us to state 1 (ₒN₁) via a goto
//    transition from the state machine.
//  * If we start in state 0, an `x` takes us to state 3 (ₒx₃)
//  * Once in state 3, an A takes us to state 1 (₃A₁)
//
// Extended grammar productions will always feature the properties that
//  * The starting state for the non-terminal is the same as the starting state
//    for the first symbol.
//  * The ending state of each symbol is the same as the starting state of the
//    next symbol.
// Note that the ending state of the last symbol is not necessarily the same as
// the ending state of the non-terminal.
//
// There may be more than one extended production for each original production,
// if that production can correspond to more than one state. For example, it
// would be perfectly valid to also have the production `₂N₅ -> ₂x₄ ₄A₅` in the
// same extended grammar as the previous example.
//
// Computing the extended grammar is simple once we have the state machine. For
// each state in the state machine, for each item where the ⋅ is at the start,
// we get an extended production where
//  * The starting state of the non-terminal is the state the item came from,
//    and the ending state is the state we would transition to if we saw that
//    non-terminal next.
//  * The starting state of the first symbol is the state the item came from,
//    and the ending state is the state we would transition to if we saw that
//    symbol next.
//  * The starting state of each subsequent symbol is the ending state of the
//    previous symbol, and the ending state is the state we would transition to
//    from the starting state of that symbol if we saw that symbol next.
// All of the information needed to compute these extended productions is easily
// accessible using the state machine.
//
////////////////////////////////////////////////////////////////////////////////
// LALR(1) Parsing, Phase III: Follow Sets
//
// The follow set of a non-terminal is the set of all the terminals which could
// follow a sequence of tokens which reduces to that non-terminal. Take this
// grammar for example:
//      1. S -> A x
//      2. S -> A B
//      3. B -> B y
//      4. B -> b
// The non-terminal A can be followed by the terminals `x` (from rule 1) and `y`
// (because A can be followed by B (2) and B can be followed by `y` (3)). So
// Follow(A) = {x, y}.
//
// We will use the follow sets to determine when to reduce using which
// productions. In the example, `Follow(A) = {x, y}` tells us that if we see an
// `x` or a `y` and we can reduce to produce an `A`, we should do so.
//
// In order to compute the follow sets of a grammar, we need to define a related
// concept: first sets. The first set of a non-terminal is the set of all the
// terminals which could start a sequence of tokens which reduces to that non-
// terminal. In the grammar above, for instance, First(B) = {b}. We will use
// first sets to express the notion that a non-terminal is followed by the first
// terminal of whatever symbol comes after it.
//
// With that in mind, here are the rules for computing follow sets:
//      1. $ ∈ Follow(S)
//      2. For each production of the form `N -> αΒβ` (where `N` and `Β` are
//         non-terminals and `α` and `β` are symbols), First(β) ∈ Follow(Β)
//      3. For each production of the form `N -> αΒ`, Follow(Β) ⊂ Follow(N).
// There are more complex rules for dealing with productions containing ε, but
// such productions are not currently supported in this implementation.
//
// These rules applied to a particular grammar may lead to recursive
// constraints. For example, the productions `A -> x B` and `B -> y A` give
// us the constraints `Follow(B) ⊂ Follow(A)` and `Follow(A) ⊂ Follow(B)`. We
// could compute the follow sets by solving this resulting system of set
// constraints (in this case learning that Follow(A) = Follow(B)). However,
// there is an easy way out: the set of constraints is always monotonic, meaning
// satisfying one constraint may break another -- but only in a way that makes
// the resulting sets larger. So, we can simply apply one rule at a time until
// we can't add any more terminals to any of the sets, and we have our follow
// sets.
//
// In order to compute the reduction actions for the parse table, we compute the
// follow sets of the extended grammar from phase II. That is the output of
// phase III, and we never actually care about the follow sets of the input
// grammar.
//
////////////////////////////////////////////////////////////////////////////////
// LALR(1) Parsing, Phase IV: Reductions and Accepting States
//
// After phase I, we had a parse table with all of the gotos and all of the
// shift actions filled out. In this phase, we finalize the table by filling out
// the accept and reduce actions.
//
// The accept actions are easy to. Any state containing an item `S -> N⋅` (where
// `S` is the start symbol) has an "accept" action on the end-of-input terminal
// $.
//
// The reductions are derived from the extended grammar and its follow sets. For
// each extended grammar production `ᵢNⱼ -> ᵢαₖ` there is a reduction by the
// rule `N -> α` in state `k` for each terminal in Follow(ᵢNⱼ). Note that we
// exclude from this process any rule in the extended grammar which is derived
// from the starting rule in the original grammar, as the corresponding actions
// for that special rule are "accept", not "reduce".
//
////////////////////////////////////////////////////////////////////////////////
// Dynamic Grammars
//
// There are two challenges when attempting to update an LALR(1) parser during
// parsing. They are:
//  1. How to efficiently update the various intermediate data structures (the
//     state machine, extended grammar, etc.) to arrive at the new parse table.
//  2. How to decide when it is safe for the parser to switch over to the new
//     parse table.
//
// This implementation does not currently do anything clever about problem 1. We
// simply rebuild the new parse table from scratch, using the updated grammar,
// when it is time to switch to the new table. It is performant enough for the
// time being, but if performance does become an issue, there is some existing
// work on the problem of incrementally updating LALR data structures (Horspool
// 1989, https://link.springer.com/content/pdf/10.1007/3-540-51364-7_10.pdf).
//
// Problem 2 can be summarized thus: at any time during parsing, the parser has
// a stack of states from the state machine which represent sets of reductions
// it might apply depending on subsequent input. If we change the grammar (and
// thus change the state machine) how can we be sure we aren't changing the
// meaning of the states on the stack?
//
// We do this using precedence. The states in the state machine are ordered
// based on the precedences of the symbols that must be encountered to reach the
// items in their kernel. Specifically, states are ordered based on the
// highest-precedence symbol in the lowest-precedence path from the starting
// state to each state. We define the precedence `Prec(i)` of a state `i` as the
// precedence of this symbol.
//
// This choice of ordering constrains how the state machine can change when new
// rules are added to the grammar: the addition of a production whose first
// symbol has precedence `p` cannot change the index or interpretation of a
// state `i` with `Prec(i) < p`, because any state whose kernel contains an item
// derived from the new production can only be reached from another state by a
// transition of precedence `p`, and therefore such a state comes after state
// `i` in the order.
//
// Let S be the multiset of the precedences of each state on the state stack at
// some point during parsing. It is clear from the above that if `max(S) < p`,
// then it is safe to add a new production that begins with `p`. There must be
// some symbol α in the partial output such that `max(S) <= Prec(α)`, since we
// must have transitioned on such a symbol (or one with a higher precedence, if
// we did not take the lowest-precedence path used to define Prec) to reach a
// state whose precedence is `max(S)`. Therefore, if all symbols in the output
// have precedence less than `p` (so, in particular, Prec(α) < p), then `max(S)
// < p`. This leads to an algorithm which does not require computing state
// precedences at all. Instead, we can just look at the precedences of symbols
// in the output.
//
// We maintain two auxiliary pieces of state (in addition to the usual state:
// input, stack of automaton states, and partial output). These are
//  1. O: The multiset of the precedences of each symbol in the output.
//  2. P: The minimum of the precedences of the first symbol of each production
//        which has been added to the grammar since we last updated the parse
//        table.
// Whenever O changes, we check if `max(O) < P`. If it is, we regenerate the
// parse table from the new grammar and switch to the new parse table.
//
// Note that there is some additional complexity involving ordering the starting
// states of the state machine. Ideally, the starting states would have very low
// precedence, since one of them will always be on the bottom of the state
// stack, and thus assigning a high precedence to the starting states would make
// it impossible to add new rules to the grammar during parsing. However, there
// is a starting state of the form `S -> N` for each non-terminal `N`, and so
// adding a new non-terminal `N` (even if the production that created it starts
// with a high-precedence symbol) would implicitly add a new low-precedence
// state to the state machine, which would be very difficult to add to the
// parsing state due to its low precedence.
//
// To get around this apparent impasse, starting states occupy a separate,
// parallel table from normal states, so their precedence is incomparable to
// that of normal states. Starting states are ordered by increasing precedence,
// just like normal states, and newly created non-terminals always have higher
// precedence than any existing non-terminal, so new starting states are always
// appended to the end of the starting state table. Futher, adding new
// productions to the state machine cannot change the ordering of the start
// states, because normal productions occupy an entirely different table. Thus,
// indices of starting states are stable under any valid addition to the
// grammar.
//
// To manage this additional complexity, there is a StateIndex abstraction,
// which wraps an index and a tag identifying the table to which the index is
// relative (the starting state table or the normal state table). In addition,
// we do still have to respect newly added non-terminals when deciding if we can
// update the parse table mid-parse, because a newly added non-terminal N
// corresponds to an implicit production `S -> N`, and the precedence of the
// first symbol of newly added productions is relevant when deciding if it is
// safe to update the parse table.
//
////////////////////////////////////////////////////////////////////////////////
// Dynamic and Context-Sensitive Lexing
//
// We allow dynamically added grammar rules to define new kinds of terminals,
// which means the lexer also needs to be dynamic. It is easy to add new tokens
// to the TokenTrie data structure used by the lexer (defined in lex.c). But
// there is a problem: these tokens are added to the trie immediately, even if
// we do not update the state machine used for parsing for some time, due to the
// precedence of the added rules.
//
// To fix this, the lexer is aware of the current state of the parser, and will
// only emit tokens which are expected in the current parser state (that is,
// tokens which lead to a shift or accept action after performing any necessary
// reductions).
//
// This behavior has an additional benefit for bl:mp, where the goal is to
// create a friendly language for implementing DSLs. One can define tokens and
// keywords which are meaningful in a particular DSL, and still use those same
// keywords as identifiers outside of the DSL, because the lexer will not treat
// them as special tokens if the parser state is not expecting those tokens.
//
// The algorithm for determining whether a token is expected in a particular
// state is conceptually simple: just run the state machine until it finishes
// reducing and reaches a shift, accept, or error action. However, the
// performance challenges that come with doing this many times for each token we
// parse make things somewhat more complicated. The optimizations are discussed
// in detail in TerminalExpected, which implements the algorithm.
//

#include "hash_map.h"
#include "hash_set.h"
#include "ordered_multiset.h"
#include "ordered_set.h"
#include "vector.h"

#include "internal/expr.h"
#include "internal/grammar.h"
#include "internal/parse.h"

////////////////////////////////////////////////////////////////////////////////
// Productions
//
// The rule
//      (Production){p, n, h, {sym1, sym2, ..., sym_n}}
// represents the production
//      p -> sym1 sym2 ... sym_n
// where `p` is the precedence of the non-terminal of the rule.
//
// Whenever a reduction is performed with parse tree fragments `tree1`, `tree2`,
// `tree_n` on the output stack, the handler `h` is invoked as:
//      h(context, {tree1, tree2, ..., tree_n})
// producing a new parse tree fragment which replaces the `n` existing fragments
// on the stack.
typedef struct {
    ProductionHandler handler;
    void *handler_arg;
    size_t index;

    NonTerminal non_terminal;

    size_t num_symbols;
    GrammarSymbol symbols[];
} Production;

// For each non-terminal `N` in a grammar, we also get a conceptual pseudo-
// terminal `pt(N)` and a production `N -> pt(N)`. This effectively defines an
// injection from non-terminals to terminals. Whenever the parser shifts a
// pseudo-terminal `pt(N)`, it will immediately reduce it to `N` and proceed
// exactly as if it had "shifted" an already-parsed parse-tree with precedence
// `N`. This allows us to parse sequences of parse trees just as easily as we
// can parse sequences of tokens, which is helpful because bl:mp macros return
// sequences of parse trees with associated precedences that need to be combined
// into a single parse tree.
//
// We represent pseudo-terminals as terminals with values greater than any
// normal terminal, so `pt(N) = max(normal terminals) + 1 + N`. Since new
// terminals can be added at any time, pseudo-terminals are only constant for a
// particular snapshot of a grammar, and thus they are associated with a state
// machine built from a grammar, not with the grammar itself. Similarly, the
// pseudo-productions `N -> pt(N)` are not stored with the rest of the grammar
// productions. They are generated and processed on the fly during the state
// machine generation process, after it processes normal productions.

// All pseudo-productions have PSEUDO_PRODUCTION_INDEX as their index field.
#define PSEUDO_PRODUCTION_INDEX ((size_t)-1)

// All pseudo-productions are handled during parsing by PseudoHandler(), which
// simply returns the top-level expression from the parse tree represented by
// the pseudo-terminal.
static Status PseudoHandler(ParserContext *ctx, ParseTree **tree)
{
    (void)ctx;

    assert(!(*tree)->grammar_symbol.is_terminal);
    assert((*tree)->num_sub_trees > 0);

    // Make a copy of the nested parse tree represented by the pseudo-terminal.
    ParseTree *sub_tree = ParseTree_Borrow((*tree)->sub_trees[0]);

    // Completely replace the output tree with the copied sub-tree.
    ParseTree_Release(*tree);
    *tree = sub_tree;

    return BLIMP_OK;
}

////////////////////////////////////////////////////////////////////////////////
// Grammar API
//

#define START_SYMBOL ((NonTerminal)0)

static bool Terminal_Eq(const Terminal *t1, const Terminal *t2, void *arg)
{
    (void)arg;
    return *t1 == *t2;
}

static size_t Terminal_Hash(const Terminal *t, void *arg)
{
    (void)arg;

    size_t hash = HASH_SEED;
    Hash_AddInteger(&hash, *t);
    return hash;
}

static bool NonTerminal_Eq(
    const NonTerminal *nt1, const NonTerminal *nt2, void *arg)
{
    (void)arg;
    return *nt1 == *nt2;
}

static size_t NonTerminal_Hash(const NonTerminal *nt, void *arg)
{
    (void)arg;

    size_t hash = HASH_SEED;
    Hash_AddInteger(&hash, *nt);
    return hash;
}

static int GrammarSymbol_Cmp(
    const GrammarSymbol *sym1, const GrammarSymbol *sym2)
{
    if (sym1->is_terminal != sym2->is_terminal) {
        return (int)sym1->is_terminal - (int)sym2->is_terminal;
    } else if (sym1->is_terminal) {
        return (int)sym1->terminal - (int)sym2->terminal;
    } else {
        return (int)sym1->non_terminal - (int)sym2->non_terminal;
    }
}

static bool GrammarSymbol_Eq(
    const GrammarSymbol *sym1, const GrammarSymbol *sym2, void *arg)
{
    (void)arg;
    return GrammarSymbol_Cmp(sym1, sym2) == 0;
}

static size_t GrammarSymbol_Hash(const GrammarSymbol *sym, void *arg)
{
    (void)arg;

    size_t hash = HASH_SEED;
    Hash_AddBoolean(&hash, sym->is_terminal);
    if (sym->is_terminal) {
        Hash_AddInteger(&hash, sym->terminal);
    } else {
        Hash_AddInteger(&hash, sym->non_terminal);
    }

    return hash;
}

// A GrammarListener can be used to get updates when a Grammar changes. Each
// Grammar keeps a list of all of its active listeners. When a new rule is
// added, the Grammar updates each of its listeners with the precedence of the
// new rule. GrammarListener_ShouldUpdate() can then be used to determine if
// data structures derived from the Grammar (like parse tables) need updating
// and if they can be safely updated.
typedef struct GrammarListener {
    GrammarSymbol min_new_precedence;
        // The minimum precedence of the first symbols of each rule added to the
        // grammar since the last successful call to
        // GrammarListener_ShouldUpdate().
        //
        // An in-progress parser can safely update its data structures as long
        // as all of the symbols on its output stack have precedence lower than
        // this.
    bool dirty;
        // Whether any new rules have been added to the grammar since the last
        // successful call to GrammarListener_ShouldUpdate().
        // `min_new_precedence` is only valid if `dirty` is `true`.

    // Each GrammarListener belongs to a doubly-linked list of listeners owned
    // by a Grammar.
    struct GrammarListener *next;
    struct GrammarListener *prev;
} GrammarListener;

static void Grammar_Listen(Grammar *grammar, GrammarListener *listener)
{
    listener->dirty = false;

    // Add the new listener to this grammar's list of listeners.
    listener->prev = NULL;
    listener->next = grammar->listeners;
    if (listener->next != NULL) {
        listener->next->prev = listener;
    }
    grammar->listeners = listener;
}

static void Grammar_Unlisten(Grammar *grammar, GrammarListener *listener)
{
    // Remove `listener` from this grammar's list of listeners.
    if (listener->next != NULL) {
        listener->next->prev = listener->prev;
    }
    if (listener->prev != NULL) {
        assert(listener != grammar->listeners);
        listener->prev->next = listener->next;
    } else {
        assert(listener == grammar->listeners);
        grammar->listeners = listener->next;
    }
}

static bool GrammarListener_ShouldUpdate(
    GrammarListener *listener, const GrammarSymbol *min_precedence)
{
    if (listener->dirty &&
        GrammarSymbol_Cmp(min_precedence, &listener->min_new_precedence) < 0)
    {
        listener->dirty = false;
        return true;
    } else {
        return false;
    }
}

static Status Grammar_AddNonTerminal(
    Grammar *grammar, NonTerminal nt, const char *name);

Status Grammar_Init(Blimp *blimp, Grammar *grammar, Terminal eof)
{
    Vector_Init(
        blimp, &grammar->productions, sizeof(Production *), FreeDestructor);
    Vector_Init(
        blimp, &grammar->productions_for_non_terminals, sizeof(Vector), NULL);
    Vector_Init(
        blimp, &grammar->terminal_strings, sizeof(char *), FreeDestructor);
    Vector_Init(
        blimp, &grammar->non_terminal_strings, sizeof(char *), FreeDestructor);

    if (HashMap_Init(
            blimp,
            &grammar->non_terminals,
            sizeof(Symbol *),
            sizeof(NonTerminal),
            (EqFunc)SymbolEq,
            (HashFunc)SymbolHash,
            NULL)
        != BLIMP_OK)
    {
        Vector_Destroy(&grammar->productions);
        Vector_Destroy(&grammar->productions_for_non_terminals);
        Vector_Destroy(&grammar->terminal_strings);
        Vector_Destroy(&grammar->non_terminal_strings);
        return Reraise(blimp);
    }

    grammar->eof_terminal = eof;
    grammar->listeners = NULL;
    grammar->parse_table = NULL;

    grammar->num_terminals = 0;
    if (Grammar_AddTerminal(grammar, NUM_BUILT_IN_TOKENS - 1) != BLIMP_OK) {
        Grammar_Destroy(grammar);
        return Reraise(blimp);
    }

    // Add the built-in start non-terminal to the grammar, so that any
    // non-terminals later added by the user will have higher precedence.
    // However, we do not want to add this non-terminal to the `non_terminals`
    // hash map, because it should not be nameable by the user, as it is
    // internal only. Therefore, we will call Grammar_AddNonTerminal directly
    // instead of calling Grammar_GetNonTerminal.
    if (Grammar_AddNonTerminal(grammar, START_SYMBOL, "Start") != BLIMP_OK) {
        Grammar_Destroy(grammar);
        return Reraise(blimp);
    }

    return BLIMP_OK;
}

void Grammar_Destroy(Grammar *grammar)
{
    Vector_Destroy(&grammar->productions);
    Vector_Destroy(&grammar->productions_for_non_terminals);
    Vector_Destroy(&grammar->terminal_strings);
    Vector_Destroy(&grammar->non_terminal_strings);
}

static inline size_t Grammar_NumProductions(const Grammar *grammar)
{
    return Vector_Length(&grammar->productions);
}

static inline Production **Grammar_Begin(const Grammar *grammar)
{
    return Vector_Begin(&grammar->productions);
}

static inline Production **Grammar_End(const Grammar *grammar)
{
    return Vector_End(&grammar->productions);
}

static inline Production **Grammar_Next(
    const Grammar *grammar, Production **production)
{
    return Vector_Next(&grammar->productions, production);
}

static inline Production *Grammar_GetProduction(
    const Grammar *grammar, size_t index)
{
    return *(Production **)Vector_Index(&grammar->productions, index);
}

static inline Vector *Grammar_ProductionsForNonTerminal(
    const Grammar *grammar, NonTerminal nt)
{
    return Vector_Index(&grammar->productions_for_non_terminals, nt);
}

static inline size_t Grammar_NumNonTerminals(const Grammar *grammar)
{
    return Vector_Length(&grammar->productions_for_non_terminals);
}

static inline size_t Grammar_NumTerminals(const Grammar *grammar)
{
    return grammar->num_terminals;
}

static struct StateMachine *StateMachine_Borrow(struct StateMachine *m);
static void StateMachine_Release(struct StateMachine *m);

static Status Grammar_AddNonTerminal(
    Grammar *grammar, NonTerminal nt, const char *name)
{
    if (nt < Grammar_NumNonTerminals(grammar)) {
        return BLIMP_OK;
    } else {
        // If any parsing operations are in progress and listening for updates
        // to the grammar, inform them of the lowest-precedence new
        // non-terminal.
        GrammarSymbol new = {
            .is_terminal = false,
            .non_terminal = Grammar_NumNonTerminals(grammar),
        };
        for (GrammarListener *listener = grammar->listeners;
             listener != NULL;
             listener = listener->next)
        {
            if (listener->dirty) {
                // The listener only cares about the new production if it is
                // lower precedence than the ones they already know about.
                if (GrammarSymbol_Cmp(
                        &new, &listener->min_new_precedence) < 0)
                {
                    listener->min_new_precedence = new;
                }
            } else {
                listener->dirty = true;
                listener->min_new_precedence = new;
            }
        }
    }

    // This operation invalidates the cached parse table.
    if (grammar->parse_table != NULL) {
        StateMachine_Release(grammar->parse_table);
        grammar->parse_table = NULL;
    }

    // This loop ensures that
    // productions_for_non_terminals[production->non_terminal] and
    // non_terminal_strings[nt] both exist by
    // appending empty Vectors to `productions_for_non_terminals` and default
    // names (unless `name` is non-NULL) to `non_terminal_strings` until both
    // are long enough.
    while (Grammar_NumNonTerminals(grammar) <= nt) {
        assert(Grammar_NumNonTerminals(grammar) ==
               Vector_Length(&grammar->productions_for_non_terminals));
        assert(Grammar_NumNonTerminals(grammar) ==
               Vector_Length(&grammar->non_terminal_strings));

        // Add an empty productions vector for the next non-terminal.
        Vector *productions;
        if (Vector_EmplaceBack(
                &grammar->productions_for_non_terminals, (void **)&productions)
            != BLIMP_OK)
        {
            return Reraise(Grammar_GetBlimp(grammar));
        }
        Vector_Init(
            Grammar_GetBlimp(grammar), productions, sizeof(size_t), NULL);

        char *string;
        if (name == NULL || Grammar_NumNonTerminals(grammar) < nt) {
            // If we were not given a name, or if this is not the ultimate non-
            // terminal we're trying to add, create a default name of the form
            // NT(precedence).
            string = malloc(8);
            if (string != NULL) {
                snprintf(string, 8, "NT(%zu)",
                    Vector_Length(&grammar->non_terminal_strings));
            }
        } else {
            // Otherwise, use the provided name.
            string = strdup(name);
        }

        if (Vector_PushBack(&grammar->non_terminal_strings, &string)
            != BLIMP_OK)
        {
            Vector_Contract(&grammar->productions_for_non_terminals, 1);
            free(string);
            return Reraise(Grammar_GetBlimp(grammar));
        }
    }

    return BLIMP_OK;
}

static Status Grammar_AddProduction(Grammar *grammar, Production *production)
{
    // This operation invalidates the cached parse table.
    if (grammar->parse_table != NULL) {
        StateMachine_Release(grammar->parse_table);
        grammar->parse_table = NULL;
    }

    Blimp *blimp = Grammar_GetBlimp(grammar);

    production->index = Vector_Length(&grammar->productions);
        // Save the index of the new production for future use.
    if (Vector_PushBack(&grammar->productions, &production) != BLIMP_OK) {
        // Append the pointer to the new production (which the caller must have
        // malloced) to the list of productions.
        free(production);
        return Reraise(blimp);
    }

    Grammar_AddNonTerminal(grammar, production->non_terminal, NULL);
        // Ensure productions_for_non_terminal[production->non_terminal] exists
        // so we can add the new production to it.

    // Now it is guaranteed that
    // productions_for_non_terminals[production->non_terminal] exists (even if
    // it is empty). Add the new production.
    Vector *productions_for_non_terminal = Vector_Index(
        &grammar->productions_for_non_terminals, production->non_terminal);
    if (Vector_PushBack(
            productions_for_non_terminal, &production->index) != BLIMP_OK)
    {
        goto error;
    }

    // If any parsing operations are in progress and listening for updates to
    // the grammar, inform them of the new production.
    const GrammarSymbol *first_sym = &production->symbols[0];
    for (GrammarListener *listener = grammar->listeners;
         listener != NULL;
         listener = listener->next)
    {
        if (listener->dirty) {
            // The listener only cares about the new production if it is lower
            // precedence than the ones they already know about.
            if (GrammarSymbol_Cmp(
                    first_sym, &listener->min_new_precedence) < 0)
            {
                listener->min_new_precedence = *first_sym;
            }
        } else {
            listener->dirty = true;
            listener->min_new_precedence = *first_sym;
        }
    }

    return BLIMP_OK;

error:
    Vector_Contract(&grammar->productions, 1);
    return Reraise(blimp);
}

Status Grammar_AddRule(
    Grammar *grammar,
    NonTerminal non_terminal,
    size_t num_symbols,
    const GrammarSymbol *symbols,
    ProductionHandler handler,
    void *handler_arg)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    assert(num_symbols > 0);

    // Productions have to live on the heap, because we frequently refer to them
    // by pointer, so their address is expected not to change. Allocate a new
    // Production and copy the given data into it.
    Production *rule;
    TRY(Malloc(
        blimp, sizeof(Production) + num_symbols*sizeof(GrammarSymbol), &rule));
    rule->non_terminal = non_terminal;
    rule->num_symbols = num_symbols;
    for (size_t i = 0; i < num_symbols; ++i) {
        rule->symbols[i] = symbols[i];
    }
    rule->handler = handler;
    rule->handler_arg = handler_arg;

    // Add the new production to the grammar.
    TRY(Grammar_AddProduction(grammar, rule));

    return BLIMP_OK;
}

Status Grammar_AddTerminal(Grammar *grammar, Terminal terminal)
{
    if (terminal < grammar->num_terminals) {
        return BLIMP_OK;
    }

    // This operation invalidates the cached parse table.
    if (grammar->parse_table != NULL) {
        StateMachine_Release(grammar->parse_table);
        grammar->parse_table = NULL;
    }

    Blimp *blimp = Grammar_GetBlimp(grammar);

    // For each new terminal `t`, create a human readable string name
    // "T(t)". These might be replaced later by the caller, if they want to
    // provide more meaningful names specific to this grammar. But this way,
    // we will always have default names if we need to pretty-print the
    // grammar or format any error messages.
    TRY(Vector_Reserve(&grammar->terminal_strings, grammar->num_terminals));
    for (Terminal t = grammar->num_terminals; t <= terminal; ++t) {
        char *string;
        TRY(Malloc(blimp, 8, &string));
        snprintf(string, 8, "T(%d)", (int)t);
        CHECK(Vector_PushBack(&grammar->terminal_strings, &string));
    }

    grammar->num_terminals = terminal + 1;
    return BLIMP_OK;
}

Status Grammar_GetNonTerminal(
    Grammar *grammar, const Symbol *sym, NonTerminal *non_terminal)
{
    HashMapEntry *entry;
    bool new_nt;
    TRY(HashMap_Emplace(&grammar->non_terminals, &sym, &entry, &new_nt));

    if (!new_nt) {
        *non_terminal = *(NonTerminal *)HashMap_GetValue(
            &grammar->non_terminals, entry);
    } else {
        *non_terminal = Grammar_NumNonTerminals(grammar);
        if (Grammar_AddNonTerminal(grammar, *non_terminal, sym->name)
                != BLIMP_OK)
        {
            HashMap_AbortEmplace(&grammar->non_terminals, entry);
            return Reraise(Grammar_GetBlimp(grammar));
        }

        *(NonTerminal *)HashMap_GetValue(&grammar->non_terminals, entry) =
            *non_terminal;
    }

    HashMap_CommitEmplace(&grammar->non_terminals, entry);
    return BLIMP_OK;
}

Status Grammar_GetNonTerminalSymbol(
    Grammar *grammar, NonTerminal non_terminal, const Symbol **sym)
{
    const char *name = *(const char **)Vector_Index(
        &grammar->non_terminal_strings, non_terminal);
    return Blimp_GetSymbol(Grammar_GetBlimp(grammar), name, sym);
}

void Grammar_SetTerminalString(
    Grammar *grammar, Terminal terminal, const char *string)
{
    char *copy = strdup(string);
    if (copy != NULL) {
        char **p = Vector_Index(&grammar->terminal_strings, terminal);
        free(*p);
        *p = copy;
    }
}

static Status Grammar_FirstSets(
    const Grammar *grammar, Vector/*<HashSet<Terminal>>*/ *firsts)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    // Initialize an empty first set for each non-terminal.
    Vector_Init(blimp, firsts, sizeof(HashSet), (Destructor)HashSet_Destroy);
    TRY(Vector_Reserve(firsts, Grammar_NumNonTerminals(grammar)));
    for (NonTerminal nt = 0; nt < Grammar_NumNonTerminals(grammar); ++nt) {
        HashSet *first;
        CHECK(Vector_EmplaceBack(firsts, (void **)&first));
        if (HashSet_Init(
                blimp,
                first,
                sizeof(Terminal),
                (EqFunc)Terminal_Eq,
                (HashFunc)Terminal_Hash,
                NULL
            ) != BLIMP_OK)
        {
            goto error;
        }
    }

    // We compute the first sets by iteratively applying the following rules to
    // fixpoint:
    //  1. For each terminal `t`, First(t) = {t}.
    //  2. For each production of the form `N -> α`, add First(α) to First(N).
    // We do not handle ε productions.
    bool progress;
    do {
        progress = false;

        for (Production **production = Grammar_Begin(grammar);
             production != Grammar_End(grammar);
             production = Grammar_Next(grammar, production))
        {
            HashSet *first = Vector_Index(firsts, (*production)->non_terminal);
            size_t first_size = HashSet_Size(first);

            const GrammarSymbol *sym = &(*production)->symbols[0];
            if (sym->is_terminal) {
                // If the first symbol of this production is a terminal, then
                // by rule 1 it's first set is just {sym->terminal}, and by rule
                // 2, we add that to `first`.
                if (HashSet_Insert(first, &sym->terminal) != BLIMP_OK) {
                    goto error;
                }
            } else {
                // Otherwise, we look up the contents of First(sym) and add them
                // to `first`.
                const HashSet *sym_first = Vector_Index(
                    firsts, sym->non_terminal);
                if (HashSet_Union(first, sym_first) != BLIMP_OK) {
                    goto error;
                }
            }

            progress |= HashSet_Size(first) > first_size;
                // We made progress if we added any new symbols to a first set.
        }
    } while (progress);

    return BLIMP_OK;

error:
    Vector_Destroy(firsts);
    return Reraise(blimp);
}

static Status Grammar_UpdateFollowSets(
    const Grammar *grammar, Vector/*<HashSet<Terminal>>*/ *follows)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    Vector firsts;
    TRY(Grammar_FirstSets(grammar, &firsts));

    // We compute the follow sets by iteratively applying the following rules to
    // fixpoint:
    // 1. For each production of the form N-> αΒβ, add everything in First(β) to
    //    the partially computed Follow(Β).
    // 2. For each production of the form N -> αΒ, add everything in the
    //    partially computed Follow(N) to Follow(Β).
    // We do not handle ε productions.
    bool progress;
    do {
        progress = false;

        for (Production **production = Grammar_Begin(grammar);
             production != Grammar_End(grammar);
             production = Grammar_Next(grammar, production))
        {
            // For each non-terminal except the last symbol in the production,
            // add the first set of the next symbol to its follow set.
            for (size_t i = 0; i + 1 < (*production)->num_symbols; ++i) {
                const GrammarSymbol *sym = &(*production)->symbols[i];
                if (sym->is_terminal) {
                    continue;
                }

                HashSet *follow = Vector_Index(follows, sym->non_terminal);
                size_t follow_size = HashSet_Size(follow);

                const GrammarSymbol *next_sym = &(*production)->symbols[i + 1];
                if (next_sym->is_terminal) {
                    // If the next symbol is a terminal, then its first set is
                    // just the terminal.
                    if (HashSet_Insert(follow, &next_sym->terminal)
                            != BLIMP_OK)
                    {
                        goto error;
                    }
                } else {
                    // Otherwise, look up the first set of the next non-terminal
                    // and add it to the follow set of this non-terminal.
                    const HashSet *next_first = Vector_Index(
                        &firsts, next_sym->non_terminal);
                    if (HashSet_Union(follow, next_first) != BLIMP_OK) {
                        goto error;
                    }
                }

                progress |= HashSet_Size(follow) > follow_size;
                    // We made progress if we added any terminals to a follow
                    // set.
            }

            // If the last symbol in the production is a non-terminal, add
            // everything in the follow set of the overall production to the
            // follow set of that non-terminal.
            const GrammarSymbol *final_sym =
                &(*production)->symbols[(*production)->num_symbols - 1];
            if (!final_sym->is_terminal) {
                HashSet *follow = Vector_Index(
                    follows, final_sym->non_terminal);
                const HashSet *production_follow = Vector_Index(
                    follows, (*production)->non_terminal);

                size_t follow_size = HashSet_Size(follow);
                if (HashSet_Union(follow, production_follow) != BLIMP_OK) {
                    goto error;
                }
                progress |= HashSet_Size(follow) > follow_size;
            }
        }
    } while (progress);

    Vector_Destroy(&firsts);
    return BLIMP_OK;

error:
    Vector_Destroy(&firsts);
    Vector_Destroy(follows);
    return Reraise(blimp);
}

static Status StateMachine_New(const Grammar *grammar, struct StateMachine **m);
static Status MakeParseTable(
    const Grammar *grammar, struct StateMachine *table);

static Status Grammar_GetParseTable(
    Grammar *grammar, struct StateMachine **parse_table)
{
    if (grammar->parse_table == NULL) {
        TRY(StateMachine_New(grammar, &grammar->parse_table));
        if (MakeParseTable(grammar, grammar->parse_table) != BLIMP_OK) {
            StateMachine_Release(grammar->parse_table);
            grammar->parse_table = NULL;
            return Reraise(Grammar_GetBlimp(grammar));
        }
    }

    *parse_table = StateMachine_Borrow(grammar->parse_table);
    return BLIMP_OK;
}

////////////////////////////////////////////////////////////////////////////////
// Parser actions

typedef enum {
    ERROR  = 0,
    ACCEPT = 1,
    REDUCE = 2,
    SHIFT  = 3,
} ActionType;

typedef size_t Action;
    // An Action is represented as a two-bit type and an almost-word of
    // associated data, stored in the high n-2 bits of the Action.

static inline Action MakeAction(ActionType type, size_t data)
{
    return (data << 2) | type;
}

static inline ActionType Action_Type(Action action)
{
    return (ActionType)(action & 3);
}

static inline size_t Action_Data(Action action)
{
    return action >> 2;
}

#define NO_ACTION ((Action)0)
    // If there is no action for a state, we treat it as an ERROR (ActionType 0)
    // with no accompanying information (Action_Data(NO_ACTION) == 0).

// Information about errors caused by conflicting actions in the same state. If
// we encounter one of these, it means the input grammar was not LALR(1);.
//
// The algorithm is willing to generate state machines containing conflict
// states, and the error will only be reported if we actually reach that state
// during parsing. These means, for example, users can define left- and right-
// associative operators with the same precedence, and an error will only be
// raised if a programmer mixes those operators without disambiguating
// parentheses.
typedef enum {
    NULL_CONFLICT = 0,
        // Uninitialized actions have type ERROR and data 0, so we reserve the
        // 0 field of the LALR_Conflict enum so that we can distinguish between
        // uninitialized errors and real conflicts. The enum values below will
        // all be non-zero.
    SHIFT_REDUCE_CONFLICT,
        // This error indicates that the input grammar is not LALR(1). It may
        // still be LR(1).
    REDUCE_REDUCE_CONFLICT,
        // If there is a reduce-reduce conflict, the grammar is not even LR(1).
} LALR_Conflict;

typedef struct StateMachine StateMachine;
static inline size_t StateMachine_NumTerminals(const StateMachine *m);
static inline size_t StateMachine_NumNonTerminals(const StateMachine *m);

static inline Status InitActions(
    Blimp *blimp, Action **actions, const StateMachine *m)
{
    // Calloc will initialize the array to zeroes. Since a 0 Action is
    // NO_ACTION, this means that every action which is not explicitly assigned
    // another action later on will result in a generic parse error if
    // encountered.
    TRY(Calloc(
        blimp,
        StateMachine_NumTerminals(m),
        sizeof(Action),
        actions));

    return BLIMP_OK;
}

static inline void SetAction(
    Action *actions,
    Terminal terminal,
    ActionType type,
    size_t data)
{
    Action action = MakeAction(type, data);

    if (actions[terminal] != NO_ACTION && actions[terminal] != action) {
        // If there is already an action set for this terminal, and it is
        // different than the one we are trying to set, then we have a conflict:
        // the grammar is not LALR(1).
        switch (Action_Type(actions[terminal])) {
            case ERROR:
                // If there is already error information for this action, leave
                // it alone.
                break;
            case SHIFT:
                assert(Action_Type(action) == REDUCE);
                    // Shift-shift conflicts should not be possible;
                actions[terminal] = MakeAction(ERROR, SHIFT_REDUCE_CONFLICT);
                break;
            case REDUCE:
            case ACCEPT: // For the purposes of error reporting, ACCEPT is a
                         // special case of REDUCE.
                if (Action_Type(action) == SHIFT) {
                    actions[terminal] = MakeAction(
                        ERROR, SHIFT_REDUCE_CONFLICT);
                } else {
                    actions[terminal] = MakeAction(
                        ERROR, REDUCE_REDUCE_CONFLICT);
                }
                break;
        }
    } else {
        actions[terminal] = action;
    }
}

static inline Status InitGotos(
    Blimp *blimp, size_t **gotos, const StateMachine *m)
{
    size_t num_gotos = StateMachine_NumNonTerminals(m);
        // Each state in the parse table has one Goto for each non-terminal.
    TRY(Malloc(blimp, num_gotos*sizeof(size_t), gotos));
    memset(*gotos, -1, num_gotos*sizeof(size_t));
        // Initialize the array to contain sentinel values of -1. If we
        // encounter one of these while parsing we will know we made a mistake.
        // Also, when we set a value for a particular goto, we can detect if it
        // already has a value (and thus there is a conflict) by checking if the
        // current value is not -1.
    return BLIMP_OK;
}

static inline Status SetGoto(
    const Grammar *grammar,
    size_t *gotos,
    NonTerminal non_terminal,
    size_t state)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    if (gotos[non_terminal] != (size_t)-1) {
        // If there is already a value for this Goto, we have a conflict. Unlike
        // conflicts in Actions, we don't try to delay error reporting for Goto
        // conflicts until we encounter the conflicted state during parsing. All
        // Goto conflicts are reduce-reduce, which means the grammar is not
        // LR(1) (that is, the grammar is pretty strange) and so these errors
        // are unlikely to cause much of an annoyance if they are reported
        // early.
        const char *nt_name = *(const char **)Vector_Index(
            &grammar->non_terminal_strings, non_terminal);
        return ErrorMsg(blimp, BLIMP_AMBIGUOUS_PARSE,
            "reduce-reduce conflict at non-terminal `%s'", nt_name);
    }

    gotos[non_terminal] = state;
    return BLIMP_OK;
}

////////////////////////////////////////////////////////////////////////////////
// Parser States
//

// An item is a production rule with a cursor (⋅) somewhere in its right-hand
// side. `cursor` here is the index of the symbol after the ⋅ ; that is, the
// next expected input.
typedef struct {
    const Production *production;
    size_t cursor;
} Item;

static int Item_Cmp(const Item *item1, const Item *item2)
{
    if (item1->production != item2->production) {
        return item1->production - item2->production;
    } else {
        return item1->cursor - item2->cursor;
    }
}

static size_t Item_Hash(const Item *item, void *arg)
{
    (void)arg;

    size_t hash = HASH_SEED;
    Hash_AddPointer(&hash, item->production);
    Hash_AddInteger(&hash, item->cursor);
    return hash;
}

static void Item_Destroy(Item *item)
{
    if (item->production->index == PSEUDO_PRODUCTION_INDEX) {
        free((void *)item->production);
    }
}

// A state is a collection of items. For ease of inspection, we break the
// items into two sets: the kernel and the closure.
//
// The kernel is represented as an ordered set because we want to ensure the
// items in it are distinct. It is ordered so that we can easily compare two
// kernels. We frequently look up states by kernel, and no two different states
// should ever have equivalent kernels. Since the kernel is ordered,
// "equivalence" of kernels amounts to element-wise equivalence of the items
// therein, which is easy to check.
//
// The closure is not represented by a set at all, it is a Vector. However, it
// is still conceptually a set because all of the items in it are distinct. This
// is guaranteed by the algorithm that adds items to the closure.
//
// A few side notes about kernels: if you look at some examples of states for
// LALR(1) grammars, you may get the idea that the kernels of each state are
// disjoint; that is, no two kernels share an item. If this were true, it would
// mean we could index states by any individual item in that state's kernel;
// rather than indexing by the kernel as a whole. That would simplify things
// because, for example, we wouldn't have to be so worried about the kernel
// being ordered. Unfortunately, this is not a property of LALR(1) grammars,
// after all. It is possible to have multiple states whose kernels share some
// (but not all) of their items. Here is an example:
//      A -> x y
//      A -> y B
//      B -> A
//      B -> x z
// If you compute the states for this grammar, you will find a state whose
// kernel is {A -> x ⋅ y} and a different state whose kernel is
// {A -> x ⋅ y, B -> x ⋅ z}.
//
// Another would-be nice property that does not hold for LALR(1) grammars is
// that all the items in a given kernel share the same prefix of symbols before
// the ⋅. This would seem to make sense, as the kernel of a state represents all
// of the items we might expect after parsing a given input, so it makes sense
// they would share the input which has been parsed to reach that item.
// Unfortunately, reductions in the input can replace some sequences of symbols
// with new non-terminals in some but not all of the items in a kernel. Here is
// an example:
//      S -> s A
//      A -> A b
//      A -> c
// The automaton for this grammar contains a state whose kernel is
// {A -> A ⋅ b, S -> s A ⋅}, containing two items with different prefixes,
// `A` and `s A`.
typedef struct {
    OrderedSet/*<Item>*/ *kernel;
        // This is a pointer to a malloced OrderedSet. It is owned by this State
        // and will be freed when the state is destroyed in State_Destroy. We
        // use a pointer instead of a value because the addresses of States move
        // around when Vectors containing them are resized, but the address of
        // the kernel should be stable since the state machine containing this
        // state will have references to the kernels of each set in an index for
        // finding states by kernel.
    Vector/*<Item>*/ closure;
    Action *actions;
    size_t *gotos;
} State;

static bool Kernel_Eq(const OrderedSet **k1, const OrderedSet **k2, void *arg)
{
    (void)arg;

    // Make sure the sets are the same size. If they're not, then they are not
    // equal. If they are, then we can do element-wise comparison.
    if (OrderedSet_Size(*k1) != OrderedSet_Size(*k2)) {
        return false;
    }

    // Compare each pair of items from (k1, k2).
    const Item *item1, *item2;
    OrderedSetIterator it1, it2;
    CHECK(OrderedSet_Iterator(*k1, &it1));
    CHECK(OrderedSet_Iterator(*k2, &it2));
    while (OrderedSet_Next(*k1, &it1, (const void **)&item1) &&
           OrderedSet_Next(*k2, &it2, (const void **)&item2))
    {
        if (Item_Cmp(item1, item2) != 0) {
            return false;
        }
    }

    return true;
}

static size_t Kernel_Hash(const OrderedSet **kernel, void *arg)
{
    (void)arg;

    size_t hash = HASH_SEED;

    Item *item;
    OrderedSetIterator it;
    CHECK(OrderedSet_Iterator(*kernel, &it));
    while (OrderedSet_Next(*kernel, &it, (const void **)&item)) {
        Hash_AddHash(&hash, Item_Hash(item, NULL));
    }

    return hash;
}

// An iterator which yields all of the items in the kernel and closure of a
// state. The `kernel` flag indicates which collection the iterator is currently
// looking at (we do the kernel first and then the closure). The union contains
// an iterator for the collection indicated by `kernel`.
typedef struct {
    bool kernel;
    union {
        OrderedSetIterator kernel_it;
        Item *closure_it;
    };
} StateIterator;

static inline StateIterator State_Iterator(const State *state)
{
    StateIterator it;
    it.kernel = true;
    CHECK(OrderedSet_Iterator(state->kernel, &it.kernel_it));
    return it;
}

static inline bool State_Next(
    const State *state, StateIterator *it, Item **item)
{
    if (it->kernel) {
        // If we're still iterating over the kernel, get the next item.
        if (OrderedSet_Next(
                state->kernel, &it->kernel_it, (const void **)item))
        {
            return true;
        }

        // If there are no more items in the kernel, switch over to the closure.
        it->kernel = false;
        it->closure_it = Vector_Begin(&state->closure);
    }

    if (it->closure_it == Vector_End(&state->closure)) {
        // If we've finished iterating over the closure, that's it.
        return false;
    }

    // Get the next item in the closure.
    *item = it->closure_it;
    it->closure_it = Vector_Next(&state->closure, it->closure_it);
    return true;
}

typedef enum {
    STARTING_STATE,
    NORMAL_STATE,
} StateType;

typedef struct {
    StateType type;
    size_t index;
} StateIndex;

static inline int StateIndex_Cmp(StateIndex i1, StateIndex i2) {
    if (i1.type != i2.type) {
        return (int)i1.type - (int)i2.type;
    } else {
        return (int)i1.index - (int)i2.index;
    }
}

static inline bool StateIndex_Eq(StateIndex i1, StateIndex i2) {
    return StateIndex_Cmp(i1, i2) == 0;
}

static inline size_t StateIndex_Hash(StateIndex i) {
    size_t hash = HASH_SEED;
    Hash_AddInteger(&hash, i.type);
    Hash_AddInteger(&hash, i.index);
    return hash;
}

static Terminal StateMachine_PseudoTerminal(
    const StateMachine *m, NonTerminal nt);

// If the cursor in `item` is before a non-terminal which is not already in
// `closure`, create items for the productions of that non-terminal, add them to
// the closure of `state`, and add the non-terminal to `closure` to prevent the
// same items from being added again later.
static Status State_AddClosureOfItem(
    State *state,
    const Grammar *grammar,
    const Item *item,
    const StateMachine *m,
    HashSet *closure)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    if (item->cursor >= item->production->num_symbols) {
        // The cursor is not before any symbols.
        return BLIMP_OK;
    }
    const GrammarSymbol *sym = &item->production->symbols[item->cursor];
    if (sym->is_terminal) {
        // The cursor is before a symbol, but not a non-terminal.
        return BLIMP_OK;
    }

    // Check if we've already added the closure of this non-terminal.
    bool found = false;
    TRY(HashSet_FindOrInsert(closure, &sym->non_terminal, &found));
    if (found) {
        return BLIMP_OK;
    }

    // Get all of the productions for this non-terminal and add them to
    // `state->closure`. Appending these productions to the closure preserves
    // the distinctness of the items in the closure, because
    //  * The closure does not yet contain any items with this non-terminal
    //    (otherwise we would have short-circuited in the check above).
    //  * All of the productions for a given terminal should be distinct -- a
    //    well-formed grammar should not contain duplicate rules.
    Vector *new_productions = Grammar_ProductionsForNonTerminal(
        grammar, sym->non_terminal);
    for (size_t *new_production = Vector_Begin(new_productions);
         new_production != Vector_End(new_productions);
         new_production = Vector_Next(new_productions, new_production))
    {
        Item new_item = {Grammar_GetProduction(grammar, *new_production), 0};
        TRY(Vector_PushBack(&state->closure, &new_item));
    }

    // Add a pseudo-production for the non-terminal: N -> pt(N) where pt(N) is a
    // pseudo-terminal representing `sym->non_terminal`. This will allow us to
    // handle parsed non-terminals as input when reparsing a sequence of parse
    // trees.
    Production *pseudo_production;
    TRY(Malloc(
        blimp,
        sizeof(Production) + 1*sizeof(GrammarSymbol),
        &pseudo_production));
    pseudo_production->non_terminal = sym->non_terminal;
    pseudo_production->handler = PseudoHandler;
    pseudo_production->index = PSEUDO_PRODUCTION_INDEX;
    pseudo_production->num_symbols = 1;
    pseudo_production->symbols[0] = (GrammarSymbol) {
        .is_terminal = true,
        .terminal = StateMachine_PseudoTerminal(m, sym->non_terminal),
    };
    Item pseudo_item = {pseudo_production, 0};
    if (Vector_PushBack(&state->closure, &pseudo_item) != BLIMP_OK) {
        Free(blimp, &pseudo_production);
        return Reraise(blimp);
    }

    return BLIMP_OK;
}

static Status State_Init(
    State *state,
    const Grammar *grammar,
    const StateMachine *m,
    OrderedSet *kernel)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    // The kernel needs to be allocated on the heap, so allocate space and move
    // the contents out of `kernel` and into `state->kernel`.
    TRY(Malloc(blimp, sizeof(OrderedSet), &state->kernel));
    OrderedSet_Move(kernel, state->kernel);

    if (InitActions(blimp, &state->actions, m) != BLIMP_OK) {
        goto err_actions;
    }
    if (InitGotos(blimp, &state->gotos, m) != BLIMP_OK) {
        goto err_gotos;
    }

    // Create an initially empty set to track which non-terminals we have
    // already added to the closure of this state. This set is only used while
    // constructing the State to ensure the items in the closure are distinct.
    // It will be destroyed when we leave this function.
    HashSet closure;
    if (HashSet_Init(
            blimp,
            &closure,
            sizeof(NonTerminal),
            (EqFunc)NonTerminal_Eq,
            (HashFunc)NonTerminal_Hash,
            NULL
        ) != BLIMP_OK)
    {
        goto err_closure;
    }

    Vector_Init(blimp, &state->closure, sizeof(Item), (Destructor)Item_Destroy);

    // Add the closure of each item in the kernel.
    Item *item;
    OrderedSetIterator it;
    CHECK(OrderedSet_Iterator(state->kernel, &it));
    while (OrderedSet_Next(state->kernel, &it, (const void **)&item)) {
        if (State_AddClosureOfItem(state, grammar, item, m, &closure)
                != BLIMP_OK)
        {
            goto error;
        }
    }

    // While there are items in the closure (including any items we just added
    // in the previous step) with the cursor in front of a new non-terminal, add
    // the productions for that non-terminal to the closure.
    for (size_t i = 0; i < Vector_Length(&state->closure); ++i) {
        Item *item = Vector_Index(&state->closure, i);
        if (State_AddClosureOfItem(state, grammar, item, m, &closure)
                != BLIMP_OK)
        {
            goto error;
        }
    }

    HashSet_Destroy(&closure);
    return BLIMP_OK;

error:
    OrderedSet_Destroy(state->kernel);
    Vector_Destroy(&state->closure);
    HashSet_Destroy(&closure);
err_closure:
    free(state->gotos);
err_gotos:
    free(state->actions);
err_actions:
    return Reraise(blimp);
}

typedef struct {
    StateIndex from_state;
    GrammarSymbol sym;
    GrammarSymbol max_sym;
    OrderedSet kernel;
} Transition;

static int Transition_Cmp(const Transition *t1, const Transition *t2)
{
    int cmp = GrammarSymbol_Cmp(&t1->max_sym, &t2->max_sym);
    if (cmp != 0) {
        return cmp;
    }

    cmp = StateIndex_Cmp(t1->from_state, t2->from_state);
    if (cmp != 0) {
        return cmp;
    }

    cmp = GrammarSymbol_Cmp(&t1->sym, &t2->sym);
    assert(cmp != 0);
    return cmp;
}

static Status State_VisitTransitions(
    State *state,
    StateIndex index,
    const GrammarSymbol *max_sym,
    OrderedSet/*<Transition>*/ *transitions)
{
    Blimp *blimp = Vector_GetBlimp(&state->closure);

    OrderedMap/*<GrammarSymbol, OrderedSet>*/ new_kernels;
        // Map from symbols to the kernel of the state we transition to on that
        // symbol. We will add to the kernels in this mapping as we process
        // items from this state, since potentially more than one item
        // transitions on the same symbol.
    OrderedMap_Init(
        blimp,
        &new_kernels,
        sizeof(GrammarSymbol),
        sizeof(OrderedSet),
        (CmpFunc)GrammarSymbol_Cmp
    );

    // For each item in the state, create a new item by feeding it the next
    // symbol it is expecting from the input, and add the new item to
    // `new_kernels[symbol]`. At the end of this process, for each symbol in
    // `new_kernels`, `new_kernels[symbol]` will be the kernel of the state
    // we would transition to from this state when given that symbol.
    Item *item;
    for (StateIterator it = State_Iterator(state);
         State_Next(state, &it, &item); )
    {
        if (item->cursor >= item->production->num_symbols) {
            // If the cursor is already at the end of the item, then there
            // are no transitions out of this state that involve this item.
            continue;
        }

        const GrammarSymbol *expected_sym =
            &item->production->symbols[item->cursor];

        // Get a reference to `new_kernels[expected_sym]`, creating a new
        // empty kernel for `expected_sym` if one does not already exist.
        OrderedMapEntry *transition;
        bool new_transition;
        if (OrderedMap_Emplace(
                &new_kernels, expected_sym, &transition, &new_transition)
            != BLIMP_OK)
        {
            goto error;
        }
        OrderedSet *transition_kernel = OrderedMap_GetValue(
            &new_kernels, transition);

        if (new_transition) {
            // If we just inserted a new kernel into `new_kernels`, we need
            // to initialize it.
            OrderedSet_Init(
                blimp,
                transition_kernel,
                sizeof(Item),
                (CmpFunc)Item_Cmp);
        }

        // Advance the cursor in `item` one position forward and add the
        // resulting item to the new kernel.
        if (OrderedSet_Insert(
                transition_kernel,
                &(Item) {item->production, item->cursor + 1})
            != BLIMP_OK)
        {
            OrderedMap_AbortEmplace(&new_kernels, transition);
            goto error;
        }
        OrderedMap_CommitEmplace(&new_kernels, transition);
    }

    // For each (symbol, kernel) pair that we just added to `new_kernels`
    // create a transition on `symbol` from this state to the state with
    // `kernel` and add it to the set of transitions that need processing.
    const GrammarSymbol *sym;
    OrderedSet *kernel;
    OrderedMapIterator it;
    CHECK(OrderedMap_Iterator(&new_kernels, &it));
    while (OrderedMap_Next(
            &new_kernels, &it, (const void **)&sym, (void **)&kernel))
    {
        if (OrderedSet_Insert(transitions, &(Transition) {
                .from_state = index,
                .kernel = *kernel,
                .sym = *sym,
                .max_sym =
                    (max_sym != NULL && GrammarSymbol_Cmp(max_sym, sym) > 0)
                        ? *max_sym
                        : *sym
            }) != BLIMP_OK)
        {
            goto error;
        }
    }

    OrderedMap_Destroy(&new_kernels);
    return BLIMP_OK;

error:
    OrderedMap_Destroy(&new_kernels);
    return Reraise(blimp);
}

static void State_Destroy(State *state)
{
    OrderedSet_Destroy(state->kernel);
    Vector_Destroy(&state->closure);
    free(state->kernel);
    free(state->actions);
    free(state->gotos);
}

static bool State_IsAccepting(const State *state)
{
    Item *item;
    OrderedSetIterator it;
    CHECK(OrderedSet_Iterator(state->kernel, &it));
    while (OrderedSet_Next(state->kernel, &it, (const void **)&item)) {
        if (item->production->non_terminal == START_SYMBOL) {
            // A state is accepting if it contains an item derived from the
            // grammar's start rule, and...
            assert(item->production->num_symbols == 1);
            if (item->cursor == 1) {
                // ...if the cursor is at the end of that item.
                return true;
            }
        }
    }

    return false;
}

static inline Action State_GetAction(const State *state, Terminal terminal)
{
    return state->actions[terminal];
}

static inline size_t State_GetGoto(
    const State *state, NonTerminal non_terminal)
{
    return state->gotos[non_terminal];
}

static Status State_GetTransition(
    const Grammar *grammar,
    const State *state,
    const GrammarSymbol *sym,
    StateIndex *next_state)
{
    // All transitions lead to normal states; there is no way to transition back
    // into a starting state.
    next_state->type = NORMAL_STATE;

    if (sym->is_terminal) {
        // Shift actions encode state transitions on non-terminals.
        Action shift = State_GetAction(state, sym->terminal);
        if (Action_Type(shift) == SHIFT) {
            next_state->index = Action_Data(shift);
            return BLIMP_OK;
        }

        // The only way the user can get an invalid transition into the state
        // machine is by creating conflicting grammar rules. In this case we
        // will report an error, but anything else is an internal logic error
        // and should fail one of these asserts:
        assert(Action_Type(shift) == ERROR);
        assert(Action_Data(shift) != 0);

        // If there is a conflict type associated with this error, we will
        // report an error message detailing the kind of conflict and suggesting
        // a possible fix.
        const char *conflict =
            Action_Data(shift) == SHIFT_REDUCE_CONFLICT
                ? "shift-reduce"
                : "reduce-reduce";

        const char *terminal_name =
            sym->terminal < grammar->num_terminals
                // If the terminal is a real terminal (not a pseudo-terminal)
                // use it's human-readable name.
                ? *(const char **)Vector_Index(
                        &grammar->terminal_strings, sym->terminal)
                // Otherwise, it is a pseudo-terminal. Use the name of the
                // corresponding non-terminal.
                : *(const char **)Vector_Index(
                        &grammar->non_terminal_strings,
                        sym->terminal - grammar->num_terminals)
                ;

        return ErrorMsg(Grammar_GetBlimp(grammar), BLIMP_AMBIGUOUS_PARSE,
            "potential ambiguous parse at input `%s' (%s conflict). "
            "Perhaps you need to add parentheses?",
            terminal_name, conflict
        );
    } else {
        // Gotos encode transitions on non-terminals.
        next_state->index = State_GetGoto(state, sym->non_terminal);
        assert(next_state->index != (size_t)-1);
        return BLIMP_OK;
    }
}

////////////////////////////////////////////////////////////////////////////////
// The LALR State Machine
//

struct StateMachine {
    Vector/*<State>*/ states;
    Vector/*<State>*/ start_states;
    HashMap/*<OrderedSet<Item> *, size_t>*/ index;
        // Lookup table to find the index of the unique NORMAL_STATE state with
        // a given kernel.
    const Grammar *grammar;
    size_t first_pseudo_terminal;
    size_t num_non_terminals;
    Production *start_production;
    size_t refcount;
};

typedef StateIndex StateMachineIterator;

static inline StateMachineIterator StateMachine_Iterator(const StateMachine *m)
{
    if (m->num_non_terminals > 1) {
        // There is at least one non-terminal besides the start symbol, which
        // means there is at least one starting state.
        return (StateIndex) {
            .type = STARTING_STATE,
                // Iterate over starting states first. After visiting all of
                // them, we will transition to normal states.
            .index = 0,
        };
    } else {
        // No starting states, just go straight to normal states.
        return (StateIndex) {
            .type = NORMAL_STATE,
            .index = 0,
        };
    }
}

static inline bool StateMachine_Next(
    const StateMachine *m, StateMachineIterator *iter, StateIndex *index)
{
    if (iter->type == NORMAL_STATE &&
        iter->index >= Vector_Length(&m->states))
    {
        // Since we iterate over starting states first and normal states second,
        // the termination condition is whether we have exhausted the normal
        // states.
        return false;
    }

    *index = *iter;
        // Visit the current state.

    switch (iter->type) {
        case STARTING_STATE: {
            ++iter->index;

            // Starting states are indexed by the top-level non-terminal that
            // they parse, representing the production `S -> N`, so there is no
            // starting state for the non-terminal S itself. Thus, the index `i`
            // really represents the non-terminal `i + 1`.
            if (iter->index + 1 >= m->num_non_terminals) {
                iter->type = NORMAL_STATE;
                iter->index = 0;
            }
            break;
        }

        case NORMAL_STATE: {
            ++iter->index;
            break;
        }
    }

    return true;
}

static inline State *StateMachine_GetState(const StateMachine *m, StateIndex i)
{
    switch (i.type) {
        case STARTING_STATE: {
            return Vector_Index(&m->start_states, i.index);
        }

        case NORMAL_STATE: {
            return Vector_Index(&m->states, i.index);
        }

        default: {
            assert(false);
            return NULL;
        }
    }
}

static inline size_t StateMachine_NumTerminals(const StateMachine *m)
{
    return m->first_pseudo_terminal + m->num_non_terminals;
        // We have one terminal for each terminal in the grammar at the time we
        // built the machine (up to `first_pseudo_terminal`), plus one
        // pseudo-terminal corresponding to each non-terminal.
}

static inline size_t StateMachine_NumNonTerminals(const StateMachine *m)
{
    return m->num_non_terminals;
}

static inline Terminal StateMachine_PseudoTerminal(
    const StateMachine *m, NonTerminal nt)
{
    return m->first_pseudo_terminal + nt;
}

static inline NonTerminal StateMachine_UnPseudoTerminal(
    const StateMachine *m, Terminal t)
{
    assert(t >= m->first_pseudo_terminal);
    return t - m->first_pseudo_terminal;
}

static inline bool StateMachine_IsPseudoTerminal(
    const StateMachine *m, Terminal t)
{
    return t >= m->first_pseudo_terminal;
}

static Status StateMachine_NewState(
    StateMachine *m,
    OrderedSet *kernel,
    const GrammarSymbol *max_sym,
    OrderedSet/*<Transition>*/ *transitions,
    StateType type,
    StateIndex *index)
{
    Vector/*<State>*/ *states =
        type == STARTING_STATE ? &m->start_states : &m->states;

    // For the caller's convenience, `index` is optional. For our
    // convenience, we will point it at a local variable if it is NULL so we
    // don't have to do any more NULL checks.
    StateIndex local_index;
    if (index == NULL) {
        index = &local_index;
    }
    index->type = type;
    index->index = Vector_Length(states);

    // Append and initialize the new state.
    State *state;
    TRY(Vector_EmplaceBack(states, (void **)&state));
    TRY(State_Init(state, m->grammar, m, kernel));

    if (type == NORMAL_STATE) {
        // Update the kernel-to-state lookup table if this is not a starting
        // state. Starting states are unreachable from other states because the
        // start non-terminal may not be referenced recursively from other
        // productions.
        assert(HashMap_Find(&m->index, &state->kernel) == NULL);
        TRY(HashMap_Update(&m->index, &state->kernel, &index->index));
    }

    // Add transitions out of this state to the set of transitions that need
    // processing.
    TRY(State_VisitTransitions(state, *index, max_sym, transitions));

    return BLIMP_OK;
}

static Status StateMachine_Init(const Grammar *grammar, StateMachine *m)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    TRY(HashMap_Init(
        blimp,
        &m->index,
        sizeof(OrderedSet *),
        sizeof(size_t),
        (EqFunc)Kernel_Eq,
        (HashFunc)Kernel_Hash,
        NULL
    ));
    Vector_Init(
        blimp, &m->states, sizeof(State), (Destructor)State_Destroy);
    Vector_Init(
        blimp, &m->start_states, sizeof(State), (Destructor)State_Destroy);
    m->grammar = grammar;
    m->first_pseudo_terminal = Grammar_NumTerminals(grammar);
    m->num_non_terminals = Grammar_NumNonTerminals(grammar);

    // `transitions` is a set of transitions to states we have not yet added, in
    // precedence order. By processing transitions and creating new states in
    // this order, and by inserting new transitions in order as we process new
    // states, we ensure that the final list of states is in precedence order.
    OrderedSet/*<Transition>*/ transitions;
    OrderedSet_Init(
        blimp, &transitions, sizeof(Transition), (CmpFunc)Transition_Cmp);

    // Create the starting states. For each non-terminal N, we get a starting
    // state with the kernel S -> N.
    for (NonTerminal nt = START_SYMBOL + 1; nt < m->num_non_terminals; ++nt) {
        // Create a production S -> N for the starting state.
        TRY(Malloc(
            blimp,
            sizeof(Production) + 1*sizeof(GrammarSymbol),
            &m->start_production));
        m->start_production->non_terminal = START_SYMBOL;
        m->start_production->handler = PseudoHandler;
        m->start_production->index = 0;
        m->start_production->num_symbols = 1;
        m->start_production->symbols[0] = (GrammarSymbol) {
            .is_terminal = false,
            .non_terminal = nt,
        };

        // Create the starting state from the initial production.
        OrderedSet initial_kernel;
        OrderedSet_Init(
            blimp, &initial_kernel, sizeof(Item), (CmpFunc)Item_Cmp);
        if (OrderedSet_Insert(
                &initial_kernel, &(Item){m->start_production, 0})
            != BLIMP_OK)
        {
            goto error;
        }
        if (StateMachine_NewState(
                m, &initial_kernel, NULL, &transitions, STARTING_STATE, NULL)
            != BLIMP_OK)
        {
            goto error;
        }
    }

    // Begin the depth-first traversal.
    while (!OrderedSet_Empty(&transitions)) {
        Transition transition;
        OrderedSet_RemoveMin(&transitions, &transition);

        const GrammarSymbol *sym = &transition.sym;
        OrderedSet *kernel = &transition.kernel;
        size_t to_index;

        // Check if there is already a state with the kernel we are trying to
        // transition to.
        size_t *to_ptr = HashMap_Find(&m->index, &kernel);
        if (to_ptr != NULL) {
            // If there is, all we have to do is note the new transition to the
            // existing state.
            to_index = *to_ptr;
            OrderedSet_Destroy(kernel);
        } else {
            // Otherwise, we create the state we're transitioning to (which will
            // cause the new state's neighbors to be added to `transitions`,
            // preparing us to continue the depth-first traversal in the next
            // iteration of this loop).
            StateIndex to;
            if (StateMachine_NewState(
                    m,
                    kernel,
                    &transition.max_sym,
                    &transitions,
                    NORMAL_STATE,
                    &to
                ) != BLIMP_OK)
            {
                goto error;
            }
            assert(to.type == NORMAL_STATE);
            to_index = to.index;
        }

        // Note the transition in the parse table row for the source state.
        State *from = StateMachine_GetState(m, transition.from_state);
        if (sym->is_terminal) {
            // Transitions on terminals are represented as shift actions.
            SetAction(from->actions, sym->terminal, SHIFT, to_index);
        } else {
            // Transitions on non-terminals are represented as gotos.
            if (SetGoto(
                    grammar, from->gotos, sym->non_terminal, to_index)
                != BLIMP_OK)
            {
                goto error;
            }
        }
    }

    OrderedSet_Destroy(&transitions);
    return BLIMP_OK;

error:
    OrderedSet_Destroy(&transitions);
    Vector_Destroy(&m->states);
    HashMap_Destroy(&m->index);
    free(m->start_production);
    return Reraise(blimp);
}

static Status StateMachine_New(const Grammar *grammar, StateMachine **m)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    TRY(Malloc(blimp, sizeof(StateMachine), m));
    if (StateMachine_Init(grammar, *m) != BLIMP_OK) {
        Free(blimp, m);
        return Reraise(blimp);
    }

    (*m)->refcount = 1;
    return BLIMP_OK;
}

static StateMachine *StateMachine_Borrow(StateMachine *m)
{
    ++m->refcount;
    return m;
}

static void StateMachine_Release(StateMachine *m)
{
    assert(m->refcount > 0);
    if (--m->refcount == 0) {
        Vector_Destroy(&m->states);
        HashMap_Destroy(&m->index);
        free(m->start_production);
        free(m);
    }
}

////////////////////////////////////////////////////////////////////////////////
// Extended grammar
//

// Recall an extended grammar symbol ᵢαⱼ is derived from a symbol α in the
// original grammar along with a state transition from states `i` to `j` on α.
// Since these state transitions refer to a determinstic automaton, the "from
// state" together with the symbol uniquely determines the "to state", so we do
// not store the "to state".
typedef struct {
    StateIndex from_state;
    GrammarSymbol symbol;
} ExtendedGrammarSymbol;

static bool ExtendedGrammarSymbol_Eq(
    const ExtendedGrammarSymbol *nt1,
    const ExtendedGrammarSymbol *nt2,
    void *arg)
{
    (void)arg;
    return StateIndex_Eq(nt1->from_state, nt2->from_state)
        && GrammarSymbol_Eq(&nt1->symbol, &nt2->symbol, NULL);
}

static size_t ExtendedGrammarSymbol_Hash(
    const ExtendedGrammarSymbol *nt, void *arg)
{
    (void)arg;

    size_t hash = HASH_SEED;
    Hash_AddHash(&hash, StateIndex_Hash(nt->from_state));
    Hash_AddHash(&hash, GrammarSymbol_Hash(&nt->symbol, NULL));
    return hash;
}

// The extended grammar of an input grammar is itself a grammar, and we
// represent it as such using a Grammar object. Each ExtendedSymbol in the
// extended grammar maps to a normal symbol in this wrapped Grammar, known as
// the projection grammar since we are projecting the extended grammar with its
// complex symbols into a simpler space. We also store mappings between symbols
// and productions which let us map back and forth between the projected grammar
// and the extended grammar.
typedef struct {
    Grammar projection;
    HashMap/*<ExtendedGrammarSymbol, GrammarSymbol>*/ project;
        // Mapping from extended symbols in the original grammar to symbols in
        // the projection grammar.
    Vector/*<ExtendedGrammarSymbol>*/ terminals;
        // Mapping from terminals in the projection grammar to extended terminal
        // symbols in the original grammar. Partial inverse of `project`.
    Vector/*<ExtendedGrammarSymbol>*/ non_terminals;
        // Mapping from non-terminals in the projection grammar to extended
        // non-terminal symbols in the original grammar. Partial inverse of
        // `project`.
    Vector/*<Production *>*/ unproject_productions;
        // Mapping from production indices in the projection grammar to
        // productions in the original grammar.
} ExtendedGrammar;

static void ExtendedGrammar_Destroy(ExtendedGrammar *grammar)
{
    Grammar_Destroy(&grammar->projection);
    HashMap_Destroy(&grammar->project);
    Vector_Destroy(&grammar->terminals);
    Vector_Destroy(&grammar->non_terminals);
    Vector_Destroy(&grammar->unproject_productions);
}

static inline size_t ExtendedGrammar_NumProductions(
    const ExtendedGrammar *grammar)
{
    return Grammar_NumProductions(&grammar->projection);
}

static inline Production *ExtendedGrammar_GetProduction(
    const ExtendedGrammar *grammar, size_t index)
{
    return Grammar_GetProduction(&grammar->projection, index);
}

static inline const Production *ExtendedGrammar_UnprojectProduction(
    const ExtendedGrammar *grammar, size_t index)
{
    return *(const Production **)Vector_Index(
        &grammar->unproject_productions, index);
}

static Status ExtendedGrammar_Project(
    ExtendedGrammar *grammar,
    const ExtendedGrammarSymbol *extended_sym,
    GrammarSymbol *sym)
{
    Blimp *blimp = Grammar_GetBlimp(&grammar->projection);

    // Get the mapping from `extended_sym` to its projection, creating a new
    // entry if necessary.
    HashMapEntry *entry;
    bool new_symbol;
    TRY(HashMap_Emplace(&grammar->project, extended_sym, &entry, &new_symbol));

    if (new_symbol) {
        // If we created a new entry, initialize `sym` with the index of a fresh
        // terminal or non-terminal, as appropriate.
        sym->is_terminal = extended_sym->symbol.is_terminal;
        if (sym->is_terminal) {
            // Create a new terminal identifier in the projection grammar.
            sym->terminal = Vector_Length(&grammar->terminals);
            if (Grammar_AddTerminal(&grammar->projection, sym->terminal)
                    != BLIMP_OK)
            {
                HashMap_AbortEmplace(&grammar->project, entry);
                return Reraise(blimp);
            }

            // Add `extended_sym` to the reverse mapping so we can recover it
            // from the projection `sym` later on.
            if (Vector_PushBack(
                    &grammar->terminals, extended_sym) != BLIMP_OK)
            {
                HashMap_AbortEmplace(&grammar->project, entry);
                    // If we failed to add it to the reverse mapping, we don't
                    // want it in the forward mapping either.
                return Reraise(blimp);
            }
        } else {
            sym->non_terminal = Vector_Length(&grammar->non_terminals);

            // Add `extended_sym` to the reverse mapping so we can recover it
            // from the projection `sym` later on.
            if (Vector_PushBack(
                    &grammar->non_terminals, extended_sym) != BLIMP_OK)
            {
                HashMap_AbortEmplace(&grammar->project, entry);
                    // If we failed to add it to the reverse mapping, we don't
                    // want it in the forward mapping either.
                return Reraise(blimp);
            }
        }

        *(GrammarSymbol *)HashMap_GetValue(&grammar->project, entry) = *sym;
            // Set the value in the projection mapping to the new symbol we just
            // initialized.
    } else {
        *sym = *(GrammarSymbol *)HashMap_GetValue(&grammar->project, entry);
            // Return the value which was already in the projection mapping.
    }

    HashMap_CommitEmplace(&grammar->project, entry);
    return BLIMP_OK;
}

static Status ExtendedGrammar_ProjectTerminal(
    ExtendedGrammar *grammar,
    StateIndex from_state,
    Terminal terminal,
    Terminal *projection)
{
    // Terminals should always project to terminals, so we can wrap `terminal`
    // in a symbol, project it, and then extract the projected terminal from the
    // projected symbol.
    GrammarSymbol sym;
    TRY(ExtendedGrammar_Project(
        grammar,
        &(ExtendedGrammarSymbol){
            from_state,
            {.is_terminal=true, .terminal=terminal},
        },
        &sym
    ));

    assert(sym.is_terminal);
    *projection = sym.terminal;
    return BLIMP_OK;
}

static Status ExtendedGrammar_ProjectNonTerminal(
    ExtendedGrammar *grammar,
    StateIndex from_state,
    NonTerminal non_terminal,
    NonTerminal *projection)
{
    // Non-terminals should always project to Non-terminals, so we can wrap
    // `non_terminal` in a symbol, project it, and then extract the projected
    // non-terminal from the projected symbol.
    GrammarSymbol sym;
    TRY(ExtendedGrammar_Project(
        grammar,
        &(ExtendedGrammarSymbol){
            from_state,
            {.is_terminal=false, .non_terminal=non_terminal},
        },
        &sym
    ));

    assert(!sym.is_terminal);
    *projection = sym.non_terminal;
    return BLIMP_OK;
}

static ExtendedGrammarSymbol *ExtendedGrammar_Unproject(
    const ExtendedGrammar *grammar, const GrammarSymbol *sym)
{
    if (sym->is_terminal) {
        return Vector_Index(&grammar->terminals, sym->terminal);
    } else {
        return Vector_Index(&grammar->non_terminals, sym->non_terminal);
    }
}

static inline Status ExtendedGrammar_FollowSets(
    const ExtendedGrammar *grammar, Vector/*<Terminal>*/ *follows)
{
    Blimp *blimp = Grammar_GetBlimp(&grammar->projection);

    // Initialize follow sets for each non-terminal. All sets are initially
    // empty except for Follow(S) (the follow set of the start symbol) which is
    // initialized to {$}.
    Vector_Init(blimp, follows, sizeof(HashSet), (Destructor)HashSet_Destroy);
    TRY(Vector_Reserve(
        follows, Grammar_NumNonTerminals(&grammar->projection)));
    for (NonTerminal nt = 0;
         nt < Grammar_NumNonTerminals(&grammar->projection);
         ++nt)
    {
        HashSet *follow;
        TRY(Vector_EmplaceBack(follows, (void **)&follow));
        TRY(HashSet_Init(
            blimp,
            follow,
            sizeof(Terminal),
            (EqFunc)Terminal_Eq,
            (HashFunc)Terminal_Hash,
            NULL
        ));

        ExtendedGrammarSymbol *sym = ExtendedGrammar_Unproject(
            grammar, &(GrammarSymbol){.is_terminal = false, .non_terminal = nt});
        assert(!sym->symbol.is_terminal);
        if (sym->symbol.non_terminal == START_SYMBOL) {
            TRY(HashSet_Insert(follow, &grammar->projection.eof_terminal));
        }
    }

    return Grammar_UpdateFollowSets(&grammar->projection, follows);
}

static Status MakeExtendedGrammar(
    const Grammar *grammar,
    const StateMachine *m,
    ExtendedGrammar *extended)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    TRY(Grammar_Init(blimp, &extended->projection, 0));
    if (HashMap_Init(
            blimp,
            &extended->project,
            sizeof(ExtendedGrammarSymbol),
            sizeof(GrammarSymbol),
            (EqFunc)ExtendedGrammarSymbol_Eq,
            (HashFunc)ExtendedGrammarSymbol_Hash,
            NULL
        ) != BLIMP_OK)
    {
        Grammar_Destroy(&extended->projection);
        return Reraise(blimp);
    }

    Vector_Init(
        blimp, &extended->unproject_productions, sizeof(Production *), NULL);
    Vector_Init(
        blimp, &extended->terminals, sizeof(ExtendedGrammarSymbol), NULL);
    Vector_Init(
        blimp, &extended->non_terminals, sizeof(ExtendedGrammarSymbol), NULL);

    // The EOF terminal should not appear explicitly in any of the productions
    // in `grammar`, but we still need to create a projected EOF terminal that
    // maps to the EOF terminal from `grammar` (since we will need to include
    // the projected EOF terminal when we make follow sets). All that matters
    // here is the Terminal-Terminal mapping. The `from_state` won't be used, so
    // {0} is fine.
    if (ExtendedGrammar_ProjectTerminal(
            extended,
            (StateIndex){0},
            grammar->eof_terminal,
            &extended->projection.eof_terminal
        ) != BLIMP_OK)
    {
        goto error;
    }

    // We derive an extended production from each item in each state in the
    // state machine where the cursor is at the start of the item.
    StateIndex state_index;
    for (StateMachineIterator iter = StateMachine_Iterator(m);
         StateMachine_Next(m, &iter, &state_index);
    ) {
        State *state = StateMachine_GetState(m, state_index);

        Item *item;
        for (StateIterator it = State_Iterator(state);
             State_Next(state, &it, &item); )
        {
            if (item->cursor != 0) {
                continue;
            }

            // Allocate a new production.
            //
            // This item is guaranteed to give us a fresh production in the
            // extended grammar. Even though we may have seen this same item
            // before in our traversal of the state machine, we have not seen
            // this item in this state, since items in states are distinct.
            // Since extended productions include information about the state
            // they start in, the same item from different states will yield two
            // different productions in the extended grammar.
            Production *production;
            if (Malloc(
                    blimp,
                    sizeof(Production)
                        + item->production->num_symbols*sizeof(GrammarSymbol),
                    &production
                ) != BLIMP_OK)
            {
                goto error;
            }

            // Get the non-terminal of the extended rule by projecting the
            // extended symbol (state_index, item->production->non_terminal).
            if (ExtendedGrammar_ProjectNonTerminal(
                    extended,
                    state_index,
                    item->production->non_terminal,
                    &production->non_terminal
                ) != BLIMP_OK)
            {
                goto error;
            }

            // Get the extended version of each symbol. We track the state we
            // are currently in, and at each symbol apply the transition from
            // the current state on that symbol to get the next state.
            StateIndex curr = state_index;
            production->num_symbols = item->production->num_symbols;
            for (size_t i = 0; i < production->num_symbols; ++i) {
                const GrammarSymbol *sym = &item->production->symbols[i];

                if (ExtendedGrammar_Project(
                        extended,
                        &(ExtendedGrammarSymbol){curr, *sym},
                        &production->symbols[i]
                    ) != BLIMP_OK)
                {
                    goto error;
                }

                if (State_GetTransition(
                        grammar, StateMachine_GetState(m, curr), sym, &curr)
                    != BLIMP_OK)
                {
                    goto error;
                }
            }

            // Add the new projected production and a reverse mapping so we can
            // recover the original production later.
            if (Grammar_AddProduction(
                    &extended->projection, production) != BLIMP_OK)
            {
                goto error;
            }
            if (Vector_PushBack(
                    &extended->unproject_productions, &item->production)
                != BLIMP_OK)
            {
                goto error;
            }
        }
    }

    return BLIMP_OK;

error:
    ExtendedGrammar_Destroy(extended);
    return Reraise(blimp);
}

static void DumpExtendedGrammar(
    FILE *file,
    const Grammar *grammar,
    const StateMachine *table,
    const ExtendedGrammar *extended)
{
    for (Production **rule = Grammar_Begin(&extended->projection);
         rule != Grammar_End(&extended->projection);
         rule = Grammar_Next(&extended->projection, rule))
    {
        const ExtendedGrammarSymbol *nt = ExtendedGrammar_Unproject(
            extended,
            &(GrammarSymbol){
                .is_terminal=false,
                .non_terminal=(*rule)->non_terminal
            }
        );
        const char *nt_name = *(const char **)Vector_Index(
            &grammar->non_terminal_strings, nt->symbol.non_terminal);
        switch (nt->from_state.type) {
            case STARTING_STATE: {
                fprintf(file, "%s[S%zu] ->", nt_name, nt->from_state.index);
                break;
            }
            case NORMAL_STATE: {
                fprintf(file, "%s[%zu] ->", nt_name, nt->from_state.index);
                break;
            }
        }

        for (size_t i = 0; i < (*rule)->num_symbols; ++i) {
            const ExtendedGrammarSymbol *sym = ExtendedGrammar_Unproject(
                extended, &(*rule)->symbols[i]);
            if (sym->symbol.is_terminal) {
                if (StateMachine_IsPseudoTerminal(
                        table, sym->symbol.terminal))
                {
                    NonTerminal nt = StateMachine_UnPseudoTerminal(
                        table, sym->symbol.terminal);
                    const char *sym_name = *(const char **)Vector_Index(
                        &grammar->non_terminal_strings, nt);
                    switch (sym->from_state.type) {
                        case STARTING_STATE: {
                            fprintf(file, " %s[S%zu]",
                                sym_name, sym->from_state.index);
                            break;
                        }
                        case NORMAL_STATE: {
                            fprintf(file, " %s[%zu]",
                                sym_name, sym->from_state.index);
                            break;
                        }
                    }
                } else {
                    const char *sym_name = *(const char **)Vector_Index(
                        &grammar->terminal_strings, sym->symbol.terminal);
                    switch (sym->from_state.type) {
                        case STARTING_STATE: {
                            fprintf(file, " %s[S%zu]",
                                sym_name, sym->from_state.index);
                            break;
                        }
                        case NORMAL_STATE: {
                            fprintf(file, " %s[%zu]",
                                sym_name, sym->from_state.index);
                            break;
                        }
                    }
                }
            } else {
                const char *sym_name = *(const char **)Vector_Index(
                    &grammar->non_terminal_strings, sym->symbol.non_terminal);
                switch (sym->from_state.type) {
                    case STARTING_STATE: {
                        fprintf(file, " %s[S%zu]",
                            sym_name, sym->from_state.index);
                        break;
                    }
                    case NORMAL_STATE: {
                        fprintf(file, " %s[%zu]",
                            sym_name, sym->from_state.index);
                        break;
                    }
                }
            }
        }

        fprintf(file, "\n");
    }
}

////////////////////////////////////////////////////////////////////////////////
// Parse table
//

static Status MakeParseTable(const Grammar *grammar, StateMachine *table) {
    Blimp *blimp = Grammar_GetBlimp(grammar);

    ExtendedGrammar extended_grammar;
    if (MakeExtendedGrammar(grammar, table, &extended_grammar) != BLIMP_OK) {
        return Reraise(blimp);
    }

    Vector follows;
    if (ExtendedGrammar_FollowSets(&extended_grammar, &follows) != BLIMP_OK) {
        ExtendedGrammar_Destroy(&extended_grammar);
        return Reraise(blimp);
    }

    // Add an ACCEPT action on the $ terminal for each accepting state.
    StateIndex i;
    for (StateMachineIterator iter = StateMachine_Iterator(table);
         StateMachine_Next(table, &iter, &i);
    ) {
        const State *state = StateMachine_GetState(table, i);
        if (State_IsAccepting(state)) {
            SetAction(state->actions, grammar->eof_terminal, ACCEPT, 0);
        }
    }

    // Add the reduction actions. For each extended reduction ᵢNⱼ -> ᵢαₖ, we get
    // a reduction by N -> α in state `k` for each terminal `t` ∈ Follow(N).
    for (size_t i = 0;
         i < ExtendedGrammar_NumProductions(&extended_grammar);
         ++i)
    {
        const Production *production = ExtendedGrammar_GetProduction(
            &extended_grammar, i);

        const HashSet *follow = Vector_Index(
            &follows, production->non_terminal);
        if (HashSet_Empty(follow)) {
            // If there is nothing in Follow(N), then there are no reductions
            // from this rule.
            continue;
        }

        // Figure out what state we are in after parsing the right-hand side of
        // this production. That will be the state reached from the starting
        // state of the final extended symbol after transitioning on that
        // symbol.
        const ExtendedGrammarSymbol *final_sym = ExtendedGrammar_Unproject(
            &extended_grammar, &production->symbols[production->num_symbols - 1]);
        State *from_state = StateMachine_GetState(table, final_sym->from_state);
        StateIndex state_index = {0};
        if (State_GetTransition(
                grammar, from_state, &final_sym->symbol, &state_index)
            != BLIMP_OK)
        {
            ExtendedGrammar_Destroy(&extended_grammar);
            Vector_Destroy(&follows);
            return BLIMP_OK;
        }
        State *state = StateMachine_GetState(table, state_index);

        const Production *original_production =
            ExtendedGrammar_UnprojectProduction(&extended_grammar, i);
        if (State_IsAccepting(state)) {
            // If this reduction would produce the start symbol, then there's no
            // need to reduce; just accept the input.
            assert(State_GetAction(state, grammar->eof_terminal) == ACCEPT);

            // Follow(S) should be {$}.
            assert(HashSet_Size(follow) == 1
                && HashSet_Contains(
                        follow, &extended_grammar.projection.eof_terminal));

            continue;
        }

        // Add reductions for each terminal in the follow set of the left-hand
        // side of the production.
        for (HashSetEntry *it = HashSet_Begin(follow);
             it != HashSet_End(follow);
             it = HashSet_Next(follow, it))
        {
            const ExtendedGrammarSymbol *terminal = ExtendedGrammar_Unproject(
                &extended_grammar,
                &(GrammarSymbol){
                    .is_terminal = true,
                    .terminal = *(Terminal *)HashSet_GetEntry(follow, it)
                }
            );
            assert(terminal->symbol.is_terminal);
            SetAction(
                state->actions,
                terminal->symbol.terminal,
                REDUCE,
                (size_t)original_production
            );
        }
    }

    ExtendedGrammar_Destroy(&extended_grammar);
    Vector_Destroy(&follows);
    return BLIMP_OK;
}

static void DumpParseTable(
    FILE *file, const Grammar *grammar, const StateMachine *m)
{
    size_t num_terminals = StateMachine_NumTerminals(m);
    size_t num_non_terminals = StateMachine_NumNonTerminals(m);

    fprintf(file, " State |");
    for (Terminal i = 0; i < num_terminals; ++i) {
        if (StateMachine_IsPseudoTerminal(m, i)) {
            const char *string = *(const char **)Vector_Index(
                &grammar->non_terminal_strings,
                StateMachine_UnPseudoTerminal(m, i));
            fprintf(file, " %-7.7s |", string);
        } else {
            const char *string = *(const char **)Vector_Index(
                &grammar->terminal_strings, i);
            fprintf(file, " %-7.7s |", string);
        }
    }
    fprintf(file, "|");
    for (NonTerminal i = 0; i < num_non_terminals; ++i) {
        const char *string = *(const char **)Vector_Index(
            &grammar->non_terminal_strings, i);
        fprintf(file, " %-7.7s |", string);
    }
    fprintf(file, "\n");

    StateIndex i;
    for (StateMachineIterator iter = StateMachine_Iterator(m);
         StateMachine_Next(m, &iter, &i);
    ) {
        const State *state = StateMachine_GetState(m, i);
        fprintf(file, "%c%-5zu |",
            i.type == STARTING_STATE ? 'S' : ' ', i.index);
        for (Terminal t = 0; t < num_terminals; ++t) {
            Action action = state->actions[t];
            switch (Action_Type(action)) {
                case SHIFT:
                    fprintf(file, "  s%.4zu  |", Action_Data(action));
                    break;
                case REDUCE: {
                    Production *production = (Production *)Action_Data(action);
                    if (production->index == PSEUDO_PRODUCTION_INDEX) {
                        fprintf(file, "   r*    |");
                    } else {
                        fprintf(file, "  r%.4zu  |", production->index);
                    }
                    break;
                }
                case ACCEPT:
                    fprintf(file, "   acc   |");
                    break;
                default:
                    fprintf(file, "         |");
                    break;
            }
        }
        fprintf(file, "|");
        for (NonTerminal nt = 0; nt < num_non_terminals; ++nt) {
            if (state->gotos[nt] != (size_t)-1) {
                fprintf(file, " %.7zu |", state->gotos[nt]);
            } else {
                fprintf(file, "         |");
            }
        }

        fprintf(file, "\n");
    }
}

void Grammar_DumpVitals(FILE *file, Grammar *grammar)
{
    StateMachine *table = NULL;
    ExtendedGrammar extended;

    if (Grammar_GetParseTable(grammar, &table) == BLIMP_OK &&
        MakeExtendedGrammar(grammar, table, &extended) == BLIMP_OK)
    {
        DumpExtendedGrammar(file, grammar, table, &extended);
        ExtendedGrammar_Destroy(&extended);
    }

    if (table != NULL) {
        DumpParseTable(file, grammar, table);
        StateMachine_Release(table);
    }
}


////////////////////////////////////////////////////////////////////////////////
// Lexing
//

static Status Lexer_LookAhead(Lexer *lex, int *c)
{
    if (lex->look_ahead_end < Vector_Length(&lex->look_ahead_chars)) {
        // If there is a character in the look-ahead buffer which has not yet
        // been processed as part of the current token, return it and increment
        // the look-ahead pointer.
        *c = *(char *)Vector_Index(
            &lex->look_ahead_chars, lex->look_ahead_end++);
        return BLIMP_OK;
    }
    if (lex->eof) {
        // If there are no extra characters in the look-ahead buffer and the
        // input stream as at end-of-file, return EOF_CHAR.
        *c = EOF_CHAR;
        return BLIMP_OK;
    }

    // Otherwise, get a character from the input stream.
    SourceLoc loc = Stream_Location(lex->input);
    TRY(Stream_Next(lex->input, c));
    if (*c == EOF_CHAR) {
        lex->eof = true;
            // Remember that the stream has reached end-of-file so we don't try
            // to read more from it.
        return BLIMP_OK;
    }

    char truncated = (char)*c;
        // If the character is not EOF_CHAR, then it is a valid `char`.

    // Add the new character to the look-ahead buffer so we remember it in case
    // we have to backtrack.
    TRY(Vector_PushBack(&lex->look_ahead_chars, &truncated));
    TRY(Vector_PushBack(&lex->look_ahead_locs, &loc));
    ++lex->look_ahead_end;

    return BLIMP_OK;
}

static Status Lexer_Consume(
    Lexer *lex,
    size_t length,
    TokenHandler handler,
    const Symbol **sym,
    SourceRange *range,
    bool peek)
{
    if (length > lex->look_ahead_end) {
        length = lex->look_ahead_end;
            // Since we don't add the EOF character to the look-ahead buffer,
            // `length` can exceed the number of buffered characters if the
            // matched token includes EOF. We will ignore the EOF character in
            // the matched symbol, since it is not a real character anyways.
    }

    assert(lex->look_ahead_end <= Vector_Length(&lex->look_ahead_chars));
    assert(Vector_Length(&lex->look_ahead_chars) ==
           Vector_Length(&lex->look_ahead_locs));

    // Get the symbol corresponding to the prefix of `look_ahead_chars` with the
    // length we're consuming.
    TRY(HandleToken(
        lex->blimp, handler, Vector_Data(&lex->look_ahead_chars), length, sym));

    if (lex->look_ahead_end == 0) {
        // If nothing is buffered, get the source location from the input
        // stream.
        assert(length == 0);
        range->start = Stream_Location(lex->input);
        range->end = range->start;
    } else {
        // Otherwise, the source range comes from the buffered locations.
        range->start = *(SourceLoc *)Vector_Index(&lex->look_ahead_locs, 0);
        if (length == 0) {
            range->end = range->start;
        } else {
            range->end = *(SourceLoc *)Vector_Index(
                &lex->look_ahead_locs, length-1);
        }
    }

    if (peek) {
        lex->peeked_len = length;
    } else {
        // Drop the characters and corresponding locations that we consumed.
        Vector_Shift(&lex->look_ahead_chars, length);
        Vector_Shift(&lex->look_ahead_locs, length);
        lex->peeked_len = 0;
    }
    lex->look_ahead_end = 0;

    return BLIMP_OK;
}

static inline void Lexer_Backtrack(Lexer *lex)
{
    lex->look_ahead_end = 0;
}

static inline size_t Lexer_LookAheadLength(const Lexer *lex)
{
    return lex->look_ahead_end;
}

static inline SourceRange Lexer_LookAheadRange(const Lexer *lex)
{
    if (lex->look_ahead_end == 0) {
        // If nothing is buffered, get the source location from the input
        // stream.
        return (SourceRange) {
            .start = Stream_Location(lex->input),
            .end   = Stream_Location(lex->input),
        };
    } else {
        // Otherwise, the source range comes from the buffered locations.
        return (SourceRange) {
            .start = *(SourceLoc *)Vector_Index(&lex->look_ahead_locs, 0),
            .end   = *(SourceLoc *)Vector_Index(
                            &lex->look_ahead_locs, lex->look_ahead_end-1),
        };
    }
}

static inline const char *Lexer_LookAheadChars(const Lexer *lex)
{
    return Vector_Data(&lex->look_ahead_chars);
}

static bool TerminalExpected(
    const StateMachine *m,
    const Vector/*<StateIndex>*/ *stack,
    Terminal t)
{
    if (t == TOK_INVALID) {
        return false;
    }
    if (t == TOK_WHITESPACE) {
        return true;
    }
    if (StateMachine_IsPseudoTerminal(m, t)) {
        return false;
    }

    // If we have a valid, non-whitespace token, we will accept it if the parser
    // is in a state where it can shift that token, after possibly performing
    // some reductions. To check this, we simply run the state machine forward
    // with the current token as input until we get to an action which is not a
    // REDUCE. SHIFT and ACCEPT actions indicate that we should accept the
    // token, while an ERROR action indicates that we are not expecting this
    // token in the current parser state (for example, maybe the token is only
    // meaningful in a particular DSL, and we are not currently parsing that
    // DSL).
    //
    // Normally, running the state machine involves mutating the stack of
    // states, as we pop states from the stack when we consume parse trees for
    // reudctions and push new states onto the stack when we reduce a new parse
    // tree. However, this function should not mutate the parser state, as we
    // are only trying to decide if we can accept a hypothetical token. We might
    // not be able to, or we might be able to but decide not to when we later
    // find a longer token. Even if we do accept the token, we will run the
    // state machine for real once we decide to accept the token.
    //
    // The obvious solution is to make a copy of the parser stack and run the
    // state machine normally, mutating the copy. However, copying the stack
    // many times for each token we shift is prohibitively expensive. And we
    // don't even need to work on the whole stack; normally, we just need a
    // small section near the top as we only perform a few reductions before
    // shifting.
    //
    // Instead, we use a clever algorithm which does not mutate the stack. The
    // algorithm is based on the following observation: the stack never grows
    // after a reduction, and we will never again observe a state which was
    // added to the stack during a reduction. This is because each reduction
    // consumes at least one state from the stack before pushing exactly one new
    // state on. The next reduction will therefore consume the new state before
    // we ever have a chance to observe it. Therefore, instead of shrinking and
    // growing the stack, we simply keep a pointer to the current, conceptual
    // "top" of the stack, which points somewhere inside of the real, immutable
    // stack. Everything below the pointer is consistent between the
    // hypothetical stack and the real stack, because even the normal, mutating
    // algorithm would not have written in that area yet. Each reduction leaves
    // the pointer alone or moves it farther down the stack, so the pointer is
    // always pointing at consistent data.
    //
    // Start the machine from the state which is currently on top of the stack.
    const StateIndex *sp = Vector_RIndex(stack, 0);
    const State *state = StateMachine_GetState(m, *sp);
    while (true) {
        Action action = state->actions[t];
        switch (Action_Type(action)) {
            case SHIFT:
            case ACCEPT:
                return true;
            case ERROR:
                return (bool)Action_Data(action);
                    // If the error represents a conflict in the grammar, we
                    // return `true` indicating to go ahead with the parse and
                    // produce this error. We only ignore tokens which are not
                    // expected at all in this parser state; unexpected token
                    // errors have a data field of 0.
            case REDUCE:
                break;
        }

        const Production *production = (const Production *)Action_Data(action);

        // Here, we would pop from the stack states corresponding to each of the
        // consumed parse tree fragments. In the immutable version of the
        // algorithm, we simply decrement the stack pointer to represent the new
        // top of the hypothetical stack.
        //
        // Before this decrement, `sp` might be pointing to an inconsistent
        // state, because we did a hypothetical push at the end of the last
        // reduction without updating the real stack. But everything below `sp`
        // is still consistent, and this operation will decrement `sp` by at
        // least 1, moving it back into the consistent region.
        assert(production->num_symbols > 0);
        sp -= production->num_symbols;
        assert(sp >= (StateIndex *)Vector_Begin(stack));

        // Popping the states corresponding to the consumed fragments leaves us
        // in the state we were in before we transitioned based on all of those
        // sub-trees. Now we once again transition out of that state, this time
        // on the non-terminal that the reduction just produced.
        const State *tmp_state = StateMachine_GetState(m, *sp);
        assert(tmp_state->gotos[production->non_terminal] != (size_t)-1);
        state = StateMachine_GetState(
            m,
            (StateIndex) {
                NORMAL_STATE,
                tmp_state->gotos[production->non_terminal]
            }
        );
        ++sp;
            // Instead of actually pushing the new state onto the stack, which
            // would corrupt the real stack, we just increment the stack pointer
            // to reflect the fact that we have pushed onto the hypothetical
            // stack. This means that the top of the hypothetical stack might
            // not be `*sp`, since `sp` points into the real stack. But we will
            // never read `*sp`, because the next reduction will decrement `sp`
            // before reading it.
    }
}

// A matcher encapsulates a path through a TokenTrie. It can be used to traverse
// a trie and extract a single result at the end.
typedef struct {
    TrieNode *curr;
    size_t curr_len;
    TrieNode *match;
    size_t match_len;
    const StateMachine *machine;
    const Vector/*<StateIndex>*/ *stack;
} Matcher;

static inline void Matcher_Init(
    Matcher *m,
    TrieNode *root,
    const StateMachine *machine,
    const Vector/*<StateIndex>*/ *stack)
{
    m->curr = root;
    m->curr_len = 1;
        // The first character of input determines what root node to use, so by
        // the time we have a root, we have already seen at least 1 character.
    m->match = NULL;
    m->match_len = 0;
    m->machine = machine;
    m->stack = stack;
}

// Advance the Matcher one character further down the trie. Returns `true` if
// the Matcher might accept more characters.
static inline bool Matcher_Next(Matcher *m, int c)
{
    if (m->curr == NULL) {
        return false;
    }

    if (TerminalExpected(m->machine, m->stack, m->curr->terminal)) {
        // If we are currently in an accepting state (including tokens which we
        // will accept internally but then skip, such as whitespace), record the
        // possible match.
        m->match = m->curr;
        m->match_len = m->curr_len;
    }

    ++m->curr_len;
    m->curr = m->curr->children[c];
    return m->curr != NULL;
}

static inline size_t Matcher_MatchLength(const Matcher *m)
{
    return m->match_len;
        // Returns 0 if there is no match.
}

static inline Terminal Matcher_MatchTerminal(const Matcher *m)
{
    return m->match->terminal;
}

static inline TokenHandler Matcher_MatchHandler(const Matcher *m)
{
    return m->match->handler;
}

static Status Lexer_Lex(
    Lexer *lex,
    Token *tok,
    const StateMachine *m,
    const Vector/*<StateIndex>*/ *stack,
    bool peek)
{
    InitStaticTokens();

    // No peeked token available, read a new token from the stream. We will keep
    // lexing and ignoring new tokens until we get to a non-whitespace token.
    do {
        int c;
        Matcher tok_match;
        Matcher sym_match;

        // Get the first character in the input, which gives us a root trie
        // node.
        TRY(Lexer_LookAhead(lex, &c));
        Matcher_Init(&tok_match, lex->tokens->nodes[c], m, stack);

        // If the first character is an identifier or operator character, then
        // we could match a non-empty prefix of the input with a symbol token.
        // We will proceed with both matching strategies (`tok_match` and
        // `sym_match`) in parallel, and use whichever one ends up being longer
        // if they both match.
        if (IsIdentifierChar(c)) {
            Matcher_Init(&sym_match, &tok_identifier, m, stack);
        } else if (IsOperatorChar(c)) {
            Matcher_Init(&sym_match, &tok_operator, m, stack);
        } else {
            Matcher_Init(&sym_match, NULL, m, stack);
        }

        // Continue reading input until neither matcher can advance further.
        do {
            TRY(Lexer_LookAhead(lex, &c));
        } while (Matcher_Next(&tok_match, c) | Matcher_Next(&sym_match, c));

        // Take the longest match.
        Matcher *match = &tok_match;
        if (Matcher_MatchLength(&sym_match) > Matcher_MatchLength(&tok_match)) {
            match = &sym_match;
        }

        if (Matcher_MatchLength(match) == 0) {
            int length = Lexer_LookAheadLength(lex);
            SourceRange range = Lexer_LookAheadRange(lex);

            Status err;
            if (length == 0) {
                err = ErrorFrom(lex->blimp, range,
                    BLIMP_UNEXPECTED_EOF, "unexpected end of input");
            } else {
                err = ErrorFrom(lex->blimp, Lexer_LookAheadRange(lex),
                    BLIMP_INVALID_CHARACTER,
                    "invalid characters '%.*s'",
                    (int)Lexer_LookAheadLength(lex),
                    Lexer_LookAheadChars(lex)
                );
            }

            Lexer_Backtrack(lex);
            return err;
        }

        // Consume the characters we matched.
        tok->type = Matcher_MatchTerminal(match);
        TRY(Lexer_Consume(
            lex,
            Matcher_MatchLength(match),
            Matcher_MatchHandler(match),
            &tok->symbol,
            &tok->range,
            peek && tok->type != TOK_WHITESPACE
        ));
    } while (tok->type == TOK_WHITESPACE);

    return BLIMP_OK;
}

static inline void Lexer_Commit(Lexer *lex)
{
    // Drop the characters and corresponding locations that we consumed.
    Vector_Shift(&lex->look_ahead_chars, lex->peeked_len);
    Vector_Shift(&lex->look_ahead_locs, lex->peeked_len);
    lex->peeked_len = 0;
}

////////////////////////////////////////////////////////////////////////////////
// Parsing
//

static Status ParseTree_NewWithGrammarSymbol(
    Blimp *blimp,
    GrammarSymbol grammar_symbol,
    Object *symbol,
    ParseTree **children,
    size_t num_children,
    const SourceRange *range,
    ParseTree **tree)
{
    TRY(Malloc(blimp, sizeof(ParseTree), tree));

    // Check if this is an untether expression. If it is, it takes flight now,
    // the instant the parse tree is created. Untether parse trees look like
    //      _7 -> {`!` <expr>}
    if (
        num_children == 2 &&
        symbol != NULL &&
        Object_Type(symbol) == OBJ_SYMBOL &&
        strcmp("_7", ((const Symbol *)symbol)->name) == 0 &&
        children[0]->grammar_symbol.is_terminal &&
        children[0]->grammar_symbol.terminal == TOK_BANG
    ) {
        // Construct an expression from the parse tree.
        Expr *expr;
        TRY(ParseTreeToExpr(blimp, children[1], &expr));
        if (BlimpExpr_Resolve(blimp, expr) != BLIMP_OK) {
            Blimp_FreeExpr(expr);
            Free(blimp, tree);
            return Reraise(blimp);
        }

        // Evaluate the expression to the object which will become the terminal
        // symbol of the newly constructed parse tree.
        Object *obj;
        if (Blimp_Eval(blimp, expr, Blimp_GlobalObject(blimp), &obj)
                != BLIMP_OK)
        {
            Blimp_FreeExpr(expr);
            Free(blimp, tree);
            return Reraise(blimp);
        }

        (*tree)->grammar_symbol = (GrammarSymbol) {
            .is_terminal = true,
            .terminal = TOK_OBJECT,
        };
        (*tree)->symbol = obj;
        (*tree)->sub_trees = NULL;
        (*tree)->num_sub_trees = 0;

        Blimp_FreeExpr(expr);
        for (ParseTree **child = children;
             child < children + num_children;
             ++child)
        {
            ParseTree_Release(*child);
        }
        free(children);
    } else {
        (*tree)->grammar_symbol = grammar_symbol;
        (*tree)->symbol = symbol != NULL ? BlimpObject_Borrow(symbol) : NULL;
        (*tree)->sub_trees = children;
        (*tree)->num_sub_trees = num_children;
    }

    (*tree)->refcount = 1;
    if (range != NULL) {
        (*tree)->range = *range;
    } else {
        (*tree)->range = (SourceRange){0};
    }
    return BLIMP_OK;
}

Status ParseTree_New(
    Blimp *blimp,
    Object *symbol,
    ParseTree **children,
    size_t num_children,
    const SourceRange *range,
    ParseTree **tree)
{
    // Convert the symbol to a grammar symbol based on whether it is expected to
    // be a terminal or a non-terminal.
    GrammarSymbol grammar_symbol;
    grammar_symbol.is_terminal = num_children == 0;
    if (grammar_symbol.is_terminal) {
        if (Object_Type(symbol) == OBJ_SYMBOL) {
            // If the token is a symbol representing the name of a non-terminal,
            // look up the non-terminal.
            Token tok;
            TRY_FROM(range, TokenTrie_GetToken(
                &blimp->tokens, ((const Symbol *)symbol)->name, &tok));
            grammar_symbol.terminal = tok.type;
        } else {
            // All non-symbol terminals are captured by TOK_OBJECT.
            grammar_symbol.terminal = TOK_OBJECT;
        }
    } else {
        // Non-terminals must be symbols, so we can look them up in our non-
        // terminal map.
        const Symbol *sym;
        TRY_FROM(range, BlimpObject_ParseSymbol(symbol, &sym));
        TRY_FROM(range, Grammar_GetNonTerminal(
            &blimp->grammar, sym, &grammar_symbol.non_terminal));
    }

    return ParseTree_NewWithGrammarSymbol(
        blimp, grammar_symbol, symbol, children, num_children, range, tree);
}

void ParseTree_Release(ParseTree *tree)
{
    assert(tree->refcount > 0);
    if (--tree->refcount == 0) {
        for (size_t i = 0; i < tree->num_sub_trees; ++i) {
            ParseTree_Release(tree->sub_trees[i]);
        }
        free(tree->sub_trees);
        if (tree->symbol != NULL) {
            BlimpObject_Release(tree->symbol);
        }
        free(tree);
    }
}

ParseTree *ParseTree_Borrow(ParseTree *tree)
{
    ++tree->refcount;
    return tree;
}

typedef struct ParseTreePrintNode {
    bool last_child;
    struct ParseTreePrintNode *next;
} ParseTreePrintNode;

static void ParseTree_Print(
    FILE *f, const ParseTree *tree, ParseTreePrintNode *parents)
{
    // We're going to print something that looks like this:
    //      root
    //      |- child1
    //      |  |- "leaf1"
    //      |- child2
    //         |- "leaf2"
    //
    // Indent to the appropriate depth based on the length of `parents`. Add |
    // characters as we go to continue to continue the "spine" descending from
    // each parent, _unless_ a parent is the last child of _their_ parent (the
    // spine for each node ends at the start of its last child).
    //
    // At the end of this loop, `self` will point to the last node in the
    // `parents` list, which is us.
    ParseTreePrintNode *self = NULL;
    for (ParseTreePrintNode *parent = parents;
         parent != NULL;
         parent = parent->next)
    {
        if (parent->next == NULL) {
            // If this is the last level of indentation, we output |- regardless
            // of whether our parent is the last child of its parent or not,
            // because this is _our_ spine with a tick mark indicating the start
            // of a new child.
            fprintf(f, "|-");
            self = parent;
        } else if (parent->last_child) {
            fprintf(f, "  ");
        } else {
            fprintf(f, "| ");
        }
    }

    // Output the symbol of this parse tree. If it is a terminal, surround it in
    // quotes so that terminals are visually distinct in the output.
    if (tree->num_sub_trees == 0) fputc('"', f);
    BlimpObject_Print(f, tree->symbol);
    if (tree->num_sub_trees == 0) fputc('"', f);
    fputc('\n', f);

    // Print our sub-trees, at the next level of indentation.
    for (size_t i = 0; i < tree->num_sub_trees; ++i) {
        ParseTreePrintNode child = {
            .last_child = i + 1 == tree->num_sub_trees,
            .next = NULL,
        };
        if (self != NULL) {
            self->next = &child;
        } else {
            parents = &child;
        }
        ParseTree_Print(f, tree->sub_trees[i], parents);
        if (self != NULL) {
            self->next = NULL;
        }
    }
}

void BlimpParseTree_Print(FILE *f, const ParseTree *tree)
{
    ParseTree_Print(f, tree, NULL);
}

ParseTree *SubTree(const ParseTree *tree, size_t index)
{
    assert(index < tree->num_sub_trees);
    return tree->sub_trees[index];
}

size_t BlimpParseTree_NumSubTrees(const ParseTree *tree)
{
    return tree->num_sub_trees;
}

Object *BlimpParseTree_Symbol(const ParseTree *tree)
{
    return tree->symbol;
}

// A ParseTreeStream is an input abstraction for the parser which yields
// ParseTrees. It can either be an adapter for a Lexer (in which case it will
// always return trivial ParseTrees whose symbols are Terminals and whose
// `parsed` expressions are just symbols representing tokens) or it can dispense
// from a vector of already-parsed Trees, some of which may be complex
// non-terminal trees with many sub-trees.
//
// The latter capability is used to implement Reparse() (which takes a sequence
// of parse trees and parses them into one larger tree), but having this common
// interface means the main parsing algorithm doesn't actually care whether it
// is reparsing or parsing directly from a Lexer.
typedef struct {
    enum {
        STREAM_LEXER,
        STREAM_VECTOR,
    } type;

    union {
        struct {
            Lexer *lexer;
            Token cache;
        } lexer;
        struct {
            const Vector/*<ParseTree *>*/ *trees;
            size_t offset;
        } vector;
    };
} ParseTreeStream;

static void ParseTreeStream_InitLexer(ParseTreeStream *stream, Lexer *lexer)
{
    stream->type = STREAM_LEXER;
    stream->lexer.lexer = lexer;
    stream->lexer.cache.type = TOK_INVALID;
}

static void ParseTreeStream_InitVector(
    ParseTreeStream *stream, const Vector/*<ParseTree *>*/ *trees)
{
    stream->type = STREAM_VECTOR;
    stream->vector.trees = trees;
    stream->vector.offset = 0;
}

static inline Blimp *ParseTreeStream_GetBlimp(ParseTreeStream *stream)
{
    switch (stream->type) {
        case STREAM_LEXER:
            return stream->lexer.lexer->blimp;
        case STREAM_VECTOR:
            return Vector_GetBlimp(stream->vector.trees);
        default:
            abort();
    }
}

static inline void ParseTreeStream_Invalidate(ParseTreeStream *stream)
{
    switch (stream->type) {
        case STREAM_LEXER:
            stream->lexer.cache.type = TOK_INVALID;
            break;
        case STREAM_VECTOR:
            break;
        default:
            abort();
    }
}

static Status ParseTreeStream_Peek(
    ParseTreeStream *stream,
    ParseTree **tree,
    const StateMachine *m,
    const Vector/*<StateIndex>*/ *stack)
{
    Blimp *blimp = ParseTreeStream_GetBlimp(stream);

    switch (stream->type) {
        case STREAM_LEXER: {
            Token tok;
            if (stream->lexer.cache.type != TOK_INVALID) {
                // If we have a cached token, just use it.
                tok = stream->lexer.cache;
            } else {
                // If there is no cached token, then either we have just shifted
                // and are now peeking at the next token, or the state machine
                // has changed since the last shift due to a macro being
                // defined, which may in turn have changed the set of expected
                // tokens, so we need to re-lex.
                //
                // In the latter case, the characters that we lexed using the
                // previous state machine are still in the lexer's buffer, since
                // we have not consumed them yet (this happens when we shift) so
                // calling back into the lexer will simply lex those same
                // characters a second time, with a different subset of expected
                // tokens.
                TRY(Lexer_Lex(stream->lexer.lexer, &tok, m, stack, true));
                stream->lexer.cache = tok;
            }

            // Convert the peeked Token to a ParseTree with a Terminal symbol.
            return ParseTree_NewWithGrammarSymbol(
                blimp,
                (GrammarSymbol){ .is_terminal = true, .terminal = tok.type },
                (Object *)tok.symbol,
                NULL, 0,
                &tok.range,
                tree);
        }

        case STREAM_VECTOR: {
            if (stream->vector.offset < Vector_Length(stream->vector.trees)) {
                // If there are more trees in the input vector, return the next
                // one.
                *tree = ParseTree_Borrow(*(ParseTree **)Vector_Index(
                        stream->vector.trees, stream->vector.offset));

                // Mimic the behavior of the lexer, where TOK_SYMBOL acts as a
                // fallback. If we were given an unexpected terminal, but we are
                // expecting a symbol, convert the terminal to a symbol.
                //
                // Note that we don't actually have to check if a symbol is
                // expected. We can be optimistic, and if it is not expected,
                // the parser will generate an error message using the normal
                // unexpected token error path.
                if ((*tree)->grammar_symbol.is_terminal &&
                    !TerminalExpected(
                        m, stack, (*tree)->grammar_symbol.terminal))
                {
                    (*tree)->grammar_symbol.terminal = TOK_SYMBOL;
                }
                return BLIMP_OK;
            } else {
                // Otherwise, return EOF.
                return ParseTree_NewWithGrammarSymbol(
                    blimp,
                    (GrammarSymbol){
                        .is_terminal = true,
                        .terminal = TOK_EOF
                    },
                    NULL,
                    NULL, 0,
                    NULL,
                    tree);
            }
        }

        default:
            abort();
    }
}

static Status ParseTreeStream_Next(
    ParseTreeStream *stream,
    const StateMachine *m,
    const Vector/*<StartState>*/ *stack)
{
    switch (stream->type) {
        case STREAM_LEXER: {
            if (stream->lexer.cache.type != TOK_INVALID) {
                // If we peeked a token, just commit to it, removing those
                // characters from the lexer's look-ahead buffer. No need to go
                // back through the lexing process since we already know what
                // token will be lexed.
                Lexer_Commit(stream->lexer.lexer);
                stream->lexer.cache.type = TOK_INVALID;
                return BLIMP_OK;
            } else {
                // Otherwise, lex a new token, but do not cache it.
                Token tok;
                return Lexer_Lex(stream->lexer.lexer, &tok, m, stack, false);
            }
        }

        case STREAM_VECTOR:
            ++stream->vector.offset;
            return BLIMP_OK;

        default:
            abort();
    }
}

static Status ParseStream(
    ParseTreeStream *stream,
    Grammar *grammar,
    NonTerminal start,
    void *parser_state,
    ParseTree **result)
{
    assert(start > START_SYMBOL);

    Blimp *blimp = ParseTreeStream_GetBlimp(stream);

    StateMachine *m;
    TRY(Grammar_GetParseTable(grammar, &m));

    // `stack` is a stack of parser state indices which is modified by SHIFT and
    // REDUCE actions.
    Vector stack;
    Vector_Init(blimp, &stack, sizeof(StateIndex), NULL);
    StateIndex start_state = {
        STARTING_STATE,
        start - 1,
            // The index of starting states does not include the start symbol
            // itself. Therefore, the index of the starting state representing
            // the non-terminal `start` is actually `start - 1`.
    };
    if (Vector_PushBack(&stack, &start_state) != BLIMP_OK) {
        goto error;
    }

    // `output` is a stack of the parse tree fragments we have parsed so far.
    // Reductions pop some number of partial trees, reduce them into a single
    // larger tree, and push that back onto `output`. At the end of parsing
    // (when we reach an ACCEPT state), `output` should contain just a single
    // expression, which is the result of parsing.
    Vector/*<ParseTree *>*/ output;
    Vector_Init(blimp, &output, sizeof(ParseTree *), ParseTreeDestructor);

    // `output_precedences` is a multiset of the symbols in `tree`, ordered by
    // precedence.
    OrderedMultiset/*<GrammarSymbol>*/ output_precedences;
    OrderedMultiset_Init(
        blimp,
        &output_precedences,
        sizeof(GrammarSymbol),
        (CmpFunc)GrammarSymbol_Cmp
    );

    GrammarListener listener;
    Grammar_Listen(grammar, &listener);

    while (true) {
        const State *state = StateMachine_GetState(
            m, *(StateIndex *)Vector_RIndex(&stack, 0));

        ParseTree *tree;
        if (ParseTreeStream_Peek(stream, &tree, m, &stack) != BLIMP_OK) {
            goto error;
        }

        Action action;
        if (tree->grammar_symbol.is_terminal) {
            action = state->actions[tree->grammar_symbol.terminal];
        } else {
            // If the input is a non-terminal, we need to convert it to a
            // terminal so we can look up the appropriate Action. We do this by
            // taking advantage of the injection from non-terminals to
            // pseudo-terminals created by the addition of pseudo-productions
            // during the construction of the state machine.
            action = state->actions[
                StateMachine_PseudoTerminal(
                    m, tree->grammar_symbol.non_terminal)
            ];
        }

        switch (Action_Type(action)) {
            case SHIFT: {
                CHECK(ParseTreeStream_Next(stream, m, &stack));
                    // Consume the tree we peeked.

                // Push the tree onto the parse tree stack.
                if (Vector_PushBack(&output, &tree) != BLIMP_OK) {
                    ParseTree_Release(tree);
                    goto error;
                }

                // Update the count of `tree.symbol` symbols in the
                // `output_precedences` multiset.
                if (OrderedMultiset_Insert(
                        &output_precedences, &tree->grammar_symbol) != BLIMP_OK)
                {
                    goto error;
                }

                // Push the next state onto the stack.
                StateIndex next_state = { NORMAL_STATE, Action_Data(action) };
                if (Vector_PushBack(&stack, &next_state) != BLIMP_OK) {
                    goto error;
                }

                // Since `output_precedences` has changed, we should check if
                // we can update the parse table with any new productions which
                // have been added by macros. We will end up updating the table
                // if the highest-precedence symbol in the output is lower
                // precedence than the first symbol of all new productions.
                const GrammarSymbol *max_precedence = OrderedMultiset_Max(
                    &output_precedences);
                if (GrammarListener_ShouldUpdate(&listener, max_precedence)) {
                    StateMachine_Release(m);
                    if (Grammar_GetParseTable(grammar, &m) != BLIMP_OK) {
                        goto error;
                    }
                }

                break;
            }

            case REDUCE: {
                const Production *production =
                    (const Production *)Action_Data(action);

                GrammarSymbol grammar_symbol = {
                    .is_terminal = false,
                    .non_terminal = production->non_terminal,
                };
                // Get the bl:mp-facing symbol corresponding to this
                // production's grammar symbol.
                const Symbol *symbol;
                if (Grammar_GetNonTerminalSymbol(
                        grammar, production->non_terminal, &symbol) != BLIMP_OK)
                {
                    goto error;
                }

                // Begin constructing the new parse tree, starting with its
                // sub-trees.
                ParseTree **sub_trees = malloc(
                    production->num_symbols*sizeof(ParseTree *));
                if (sub_trees == NULL) {
                    Error(blimp, BLIMP_OUT_OF_MEMORY);
                    goto error;
                }
                // Get the parse tree fragments which this reduction will
                // consume from the parse tree stack.
                Vector_ContractOut(&output, production->num_symbols, sub_trees);
                // Set the source range to encompass the range of all the sub-
                // trees.
                SourceRange range = {
                    .start = sub_trees[0]->range.start,
                    .end = sub_trees[production->num_symbols-1]->range.end,
                };
                ParseTree *fragment;
                if (ParseTree_NewWithGrammarSymbol(
                        blimp,
                        grammar_symbol,
                        (Object *)symbol,
                        sub_trees,
                        production->num_symbols,
                        &range,
                        &fragment
                    ) != BLIMP_OK)
                {
                    goto error;
                }

                // Remove the corresponding symbols from `output_precedences`
                // for each expression consumed from `output`.
                for (size_t i = 0; i < production->num_symbols; ++i) {
                    OrderedMultiset_Remove(
                        &output_precedences, &production->symbols[i]);
                }

                // Call the handler to get a larger parse tree.
                ParserContext ctx = {
                    .blimp = blimp,
                    .parser_state = parser_state,
                    .arg = production->handler_arg,
                    .range = &fragment->range
                };
                if (production->handler != NULL) {
                    if (production->handler(&ctx, &fragment) != BLIMP_OK) {
                        ParseTree_Release(fragment);
                        goto error;
                    }
                }

                // Push the new expression onto the output stack, in place of
                // sub-trees taht we removed.
                assert(production->num_symbols > 0);
                CHECK(Vector_PushBack(&output, &fragment));
                    // CHECK not TRY, because we just removed at least one
                    // element from the vector, so this should not cause an
                    // allocation that could fail.

                // Add the symbol we just reduced to `output_precedences`.
                if (OrderedMultiset_Insert(
                        &output_precedences, &grammar_symbol) != BLIMP_OK)
                {
                    goto error;
                }

                // Remove the states corresponding to each of the consumed parse
                // tree fragments.
                Vector_Contract(&stack, production->num_symbols);

                // That leaves us in the state we were in before we transitioned
                // based on all of those sub-trees. Now we once again transition
                // out of that state, this time on the non-terminal that the
                // reduction just produced.
                const State *tmp_state = StateMachine_GetState(
                    m, *(StateIndex *)Vector_RIndex(&stack, 0));
                assert(tmp_state->gotos[production->non_terminal] != (size_t)-1);
                if (Vector_PushBack(&stack,
                        &(StateIndex) {
                            NORMAL_STATE,
                            tmp_state->gotos[production->non_terminal]
                        })
                    != BLIMP_OK)
                {
                    goto error;
                }

                // `output_precedences` has changed, so we need to check if we
                // should update the parse table.
                const GrammarSymbol *max_precedence = OrderedMultiset_Max(
                    &output_precedences);
                if (GrammarListener_ShouldUpdate(&listener, max_precedence)) {
                    StateMachine_Release(m);
                    if (Grammar_GetParseTable(grammar, &m) != BLIMP_OK) {
                        goto error;
                    }
                    ParseTreeStream_Invalidate(stream);
                }

                break;
            }

            case ACCEPT: {
                // Get the result from the parse tree.
                assert(tree->grammar_symbol.is_terminal);
                assert(tree->grammar_symbol.terminal == TOK_EOF);
                assert(Vector_Length(&output) == 1);
                Vector_PopBack(&output, result);

                ParseTree_Release(tree);
                Grammar_Unlisten(grammar, &listener);
                Vector_Destroy(&stack);
                Vector_Destroy(&output);
                OrderedMultiset_Destroy(&output_precedences);
                StateMachine_Release(m);
                return BLIMP_OK;
            }

            case ERROR: {
                if (Action_Data(action)) {
                    // If there is a conflict type associated with this error,
                    // we will report an error message detailing the kind of
                    // conflict and suggesting a possible fix.
                    const char *conflict =
                        Action_Data(action) == SHIFT_REDUCE_CONFLICT
                            ? "shift-reduce"
                            : "reduce-reduce";

                    if (tree->grammar_symbol.is_terminal) {
                        // If the input that caused the error is a terminal,
                        // include it in the error message.
                        const char *name;
                        if (tree->symbol == NULL) {
                            // NULL usually means EOF.
                            if (tree->grammar_symbol.terminal == TOK_EOF)
                            {
                                name = "EOF";
                            } else {
                                name = "???";
                            }
                        } else {
                            if (Object_Type(tree->symbol) == OBJ_SYMBOL) {
                                name = ((const Symbol *)tree->symbol)->name;
                            } else {
                                name = "<object literal>";
                            }
                        }
                        ErrorFrom(blimp, tree->range,
                            BLIMP_AMBIGUOUS_PARSE,
                            "ambiguous parse at input `%s' (%s conflict). "
                            "Perhaps you need to add parentheses?",
                            name, conflict
                        );
                    } else {
                        ErrorFrom(blimp, tree->range,
                            BLIMP_AMBIGUOUS_PARSE,
                            "ambiguous parse at expression with precedence %zu "
                            "(%s conflict). Perhaps you need to add "
                            "parentheses?",
                            tree->grammar_symbol.non_terminal, conflict);
                    }
                } else if (tree->grammar_symbol.is_terminal &&
                           tree->grammar_symbol.terminal == grammar->eof_terminal)
                {
                    // If this is not a grammar conflict (it's just an
                    // unexpected input) and the unexpected symbol was EOF, we
                    // have a special error message. The automatic line
                    // continuation feature of the REPL relies on a unique error
                    // code being returned when the unexpected input was EOF.
                    ErrorFrom(blimp, tree->range,
                        BLIMP_UNEXPECTED_EOF,
                        "unexpected end of input");
                } else if (tree->grammar_symbol.is_terminal) {
                    // If the unexpected input was a terminal, include it in the
                    // output.
                    const char *name =
                        Object_Type(tree->symbol) == OBJ_SYMBOL
                            ? ((const Symbol *)tree->symbol)->name
                            : "<object literal>";
                    ErrorFrom(blimp, tree->range,
                        BLIMP_UNEXPECTED_TOKEN,
                        "unexpected token `%s'", name);
                } else {
                    // Otherwise, include the name of the unexpected
                    // non-terminal.
                    const char *name = *(const char **)Vector_Index(
                        &grammar->non_terminal_strings,
                        tree->grammar_symbol.non_terminal);
                    ErrorFrom(blimp, tree->range,
                        BLIMP_UNEXPECTED_TOKEN,
                        "unexpected expression with non-terminal %s", name);
                }
                goto error;
            }
        }
    }

error:
    Grammar_Unlisten(grammar, &listener);
    Vector_Destroy(&stack);
    Vector_Destroy(&output);
    OrderedMultiset_Destroy(&output_precedences);
    StateMachine_Release(m);
    return Reraise(blimp);
}

Status Parse(
    Lexer *lex,
    Grammar *grammar,
    NonTerminal target,
    void *parser_state,
    ParseTree **parsed)
{
    ParseTreeStream stream;
    ParseTreeStream_InitLexer(&stream, lex);
    return ParseStream(&stream, grammar, target, parser_state, parsed);
}

Status Reparse(
    const Vector/*<ParseTree *>*/ *input,
    Grammar *grammar,
    NonTerminal target,
    void *parser_state,
    ParseTree **parsed)
{
    ParseTreeStream stream;
    ParseTreeStream_InitVector(&stream, input);
    return ParseStream(&stream, grammar, target, parser_state, parsed);
}
