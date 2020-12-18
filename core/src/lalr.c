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
// The process is straightforward: begin with the item `S -> ⋅sym` (determined
// by taking the grammar's unique starting rule and placing a ⋅ at the start of
// the production). Construct the closure of the state with that kernel. Perhaps
// it looks something like this:
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
// the end of this phase we actually end up with a partially filled out parser
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
// We do this using precedence. First, the states in the state machine are
// ordered based on the precedences of the symbols that must be encountered to
// reach the items in their kernel. The starting state (whose kernel is always
// {S -> ⋅N} for some non-terminal N) comes first. Then comes the state we
// transition to from the start state given the lowest-precedence non-terminal.
// Then come all the states we can reach from that state, followed by the state
// reached from the start state given the second lowest precedence non-terminal.
// In other words, the order of states is a depth-first, weighted topological
// sorting starting from the start state.
//
// We can now define the precedence of a state by the highest-precedence symbol
// in the lowest-precedence path from the start state to that state (that is,
// the path taken by the weighted depth-first traversal described above). This
// gives us the crucial result: the addition of a production whose first symbol
// has precedence `p` cannot change the index or interpretation of a state `i`
// with `Prec(i) < p`, because any state whose kernel contains an item derived
// from the new production can only be reached from another state by a
// transition of precedence `p`, and therefore such a state comes after state
// `i` in the depth-first traversal.
//
// Let S be the multiset of the precedences of each state on the state stack at
// some point during parsing. It is clear from the above that if `max(S) < p`,
// then it is safe to add a new production that begins with `p`. There must be
// some symbol α in the partial output such that `Prec(α) = max(S)`, since we
// must have transitioned on such a symbol to reach a state whose precedence is
// `max(S)`. Therefore, if all symbols in the output of precedence less than
// `p`, then `max(S) < p`. This leads to an algorithm which does not require
// computing state precedences at all. Instead, we can just look at the
// precedences of symbols in the output.
//
// We maintain two auxiliary pieces of state (in addition to the usual state:
// input, stack of automaton states, and partial output). These are
//  1. O: The multiset of the precedences of each symbol in the output.
//  2. P: The multiset of the precedences of the first symbol of each production
//        which has been added to the grammar since we last updated the parse
//        table.
// Whenever O or P changes, we check if `max(O) < min(P)`. If it is, we
// regenerate the parse table from the new grammar and switch to the new parse
// table.
//

#include "hash_map.h"
#include "hash_set.h"
#include "ordered_multiset.h"
#include "ordered_set.h"
#include "vector.h"

#include "internal/expr.h"
#include "internal/parse.h"

////////////////////////////////////////////////////////////////////////////////
// Grammar API
//

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

typedef struct GrammarListener {
    struct GrammarListener *next;
    struct GrammarListener *prev;
    GrammarSymbol min_new_precedence;
    bool dirty;
} GrammarListener;

static void Grammar_Listen(Grammar *grammar, GrammarListener *listener)
{
    listener->dirty = false;

    listener->prev = NULL;
    listener->next = grammar->listeners;
    if (listener->next != NULL) {
        listener->next->prev = listener;
    }
    grammar->listeners = listener;
}

static void Grammar_Unlisten(Grammar *grammar, GrammarListener *listener)
{
    if (listener->next != NULL) {
        listener->next->prev = listener->prev;
    }
    if (listener->prev != NULL) {
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
    grammar->num_terminals = NUM_TOKEN_TYPES;
    grammar->eof_terminal = eof;
    grammar->listeners = NULL;

    // For each terminal `t`, create a human readable string name "T(t)". These
    // might be replaced later by the caller, if they want to provide more
    // meaningful names specific to this grammar. But this way, we will always
    // have default names if we need to pretty-print the grammar or format any
    // error messages.
    TRY(Vector_Reserve(&grammar->terminal_strings, grammar->num_terminals));
    for (Terminal t = 0; t < grammar->num_terminals; ++t) {
        char *string;
        TRY(Malloc(blimp, 8, &string));
        snprintf(string, 8, "T(%d)", (int)t);
        CHECK(Vector_PushBack(&grammar->terminal_strings, &string));
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

static inline Blimp *Grammar_GetBlimp(const Grammar *grammar)
{
    return Vector_GetBlimp(&grammar->productions);
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

static Status Grammar_AddProduction(Grammar *grammar, Production *production)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    production->index = Vector_Length(&grammar->productions);
        // Save the index of the new production for future use.
    if (Vector_PushBack(&grammar->productions, &production) != BLIMP_OK) {
        // Append the pointer to the new production (which the caller must have
        // malloced) to the list of productions.
        free(production);
        return Reraise(blimp);
    }

    // We want to append the new production to
    // productions_for_non_terminals[production->non_terminal]. However, this
    // may be the first time we're seeing a production with this non-terminal,
    // so there might not yet be a corresponding entry in
    // `productions_for_non_terminals`. Worse, we might be given rules out of
    // order, so it's possible there aren't even entries for lower-precedence
    // non-terminals.
    //
    // This loop ensures that
    // productions_for_non_terminals[production->non_terminal] exists by
    // appending empty Vectors to `productions_for_non_terminals` until it is
    // long enough.
    //
    // As we do this, we will also add default human-readable non-terminal names
    // to `non_terminal_strings`, to maintain the invariant that
    // `non_terminal_strings` and `productions_for_non_terminals` have the same
    // length.
    while (Vector_Length(&grammar->productions_for_non_terminals)
           <= production->non_terminal)
    {
        assert(Vector_Length(&grammar->productions_for_non_terminals) ==
               Vector_Length(&grammar->non_terminal_strings));

        // Add an empty productions vector for the next non-terminal.
        Vector *productions;
        if (Vector_EmplaceBack(
                &grammar->productions_for_non_terminals, (void **)&productions)
            != BLIMP_OK)
        {
            goto error;
        }
        Vector_Init(blimp, productions, sizeof(size_t), NULL);

        // Add a name of the form NT(precedence) for the next non-terminal.
        char *string = malloc(8);
        if (string != NULL) {
            snprintf(string, 8, "NT(%zu)",
                Vector_Length(&grammar->non_terminal_strings));
        }
        if (Vector_PushBack(&grammar->non_terminal_strings, &string)
                != BLIMP_OK)
        {
            Vector_Contract(&grammar->productions_for_non_terminals, 1);
            goto error;
        }
    }

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

void Grammar_SetNonTerminalString(
    Grammar *grammar, NonTerminal non_terminal, const char *string)
{
    char *copy = strdup(string);
    if (copy != NULL) {
        char **p = Vector_Index(
            &grammar->non_terminal_strings, non_terminal);
        free(*p);
        *p = copy;
    }
}

static Status Grammar_FirstSets(
    const Grammar *grammar, Vector/*<HashSet<Terminal>>*/ *firsts)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    // Initialize an empty fist set for each non-terminal.
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

static Status Grammar_FollowSets(
    const Grammar *grammar, Vector/*<HashSet<Terminal>>*/ *follows)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    Vector firsts;
    TRY(Grammar_FirstSets(grammar, &firsts));

    // Initialize follow sets for each non-terminal. All sets are initially
    // empty except for Follow(S) (the follow set of the start symbol) which is
    // initialized to {$}.
    Vector_Init(blimp, follows, sizeof(HashSet), (Destructor)HashSet_Destroy);
    if (Vector_Reserve(follows, Grammar_NumNonTerminals(grammar)) != BLIMP_OK) {
        Vector_Destroy(&firsts);
        return Reraise(blimp);
    }
    for (NonTerminal nt = 0; nt < Grammar_NumNonTerminals(grammar); ++nt) {
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

        if (nt == START_SYMBOL) {
            TRY(HashSet_Insert(follow, &grammar->eof_terminal));
        }
    }

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

static inline Status InitActions(
    Blimp *blimp, Action **actions, const Grammar *grammar)
{
    TRY(Calloc(blimp, Grammar_NumTerminals(grammar), sizeof(Action), actions));
        // Each state in the parse table has one Action for each terminal.
        // Calloc will initialize the array to zeroes. Since a 0 Action is
        // ERROR, this means that every action which is not explicitly assigned
        // another action later on will result in a parse error if encountered.
    return BLIMP_OK;
}

static inline Status SetAction(
    const Grammar *grammar,
    Action *actions,
    Terminal terminal,
    ActionType type,
    size_t data)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);
    Action action = MakeAction(type, data);

    if (actions[terminal] != ERROR && actions[terminal] != action) {
        // If there is already an action set for this terminal, and it is
        // different than the one we are trying to set, then we have a conflict:
        // the grammar is not LALR(1).
        const char *terminal_name = *(const char **)Vector_Index(
            &grammar->terminal_strings, terminal);
        return ErrorMsg(blimp, BLIMP_AMBIGUOUS_PARSE,
            "%s-%s conflict at terminal %s",
            Action_Type(actions[terminal]) == REDUCE ? "reduce" : "shift",
            type == REDUCE ? "reduce" : "shift", terminal_name);
    }

    actions[terminal] = action;
    return BLIMP_OK;
}

static inline Status InitGotos(
    Blimp *blimp, size_t **gotos, const Grammar *grammar)
{
    size_t num_gotos = Grammar_NumTerminals(grammar);
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
        // If there is already a value for this Goto, we have a conflict.
        const char *string = Vector_Index(
            &grammar->non_terminal_strings, non_terminal);
        return ErrorMsg(blimp, BLIMP_AMBIGUOUS_PARSE,
            "reduce-reduce conflict at non-terminal %s", string);
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

// If the cursor in `item` is before a non-terminal which is not already in
// `closure`, create items for the productions of that non-terminal, add them to
// the closure of `state`, and add the non-terminal to `closure` to prevent the
// same items from being added again later.
static Status State_AddClosureOfItem(
    State *state,
    const Grammar *grammar,
    const Item *item,
    HashSet *closure)
{
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
    bool found;
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

    return BLIMP_OK;
}

static Status State_Init(
    State *state, const Grammar *grammar, OrderedSet *kernel)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    // The kernel needs to be allocated on the heap, so allocate space and move
    // the contents out of `kernel` and into `state->kernel`.
    TRY(Malloc(blimp, sizeof(OrderedSet), &state->kernel));
    OrderedSet_Move(kernel, state->kernel);

    if (InitActions(blimp, &state->actions, grammar) != BLIMP_OK) {
        goto err_actions;
    }
    if (InitGotos(blimp, &state->gotos, grammar) != BLIMP_OK) {
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

    Vector_Init(blimp, &state->closure, sizeof(Item), NULL);

    // Add the closure of each item in the kernel.
    Item *item;
    OrderedSetIterator it;
    CHECK(OrderedSet_Iterator(state->kernel, &it));
    while (OrderedSet_Next(state->kernel, &it, (const void **)&item)) {
        if (State_AddClosureOfItem(state, grammar, item, &closure)
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
        if (State_AddClosureOfItem(state, grammar, item, &closure) != BLIMP_OK)
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
    size_t from_state;
    GrammarSymbol sym;
    OrderedSet kernel;
} Transition;

static Status State_VisitTransitions(
    State *state, size_t index, Vector/*<Transition>*/ *dfs_stack)
{
    Blimp *blimp = Vector_GetBlimp(&state->closure);

    OrderedMap/*<GrammarSymbol, OrderedSet>*/ transitions;
        // Map from symbols to the kernel of the state we transition to on that
        // symbol. We will add to the kernels in this mapping as we process
        // items from this state, since potentially more than one item
        // transitions on the same symbol.
    OrderedMap_Init(
        blimp,
        &transitions,
        sizeof(GrammarSymbol),
        sizeof(OrderedSet),
        (CmpFunc)GrammarSymbol_Cmp
    );

    // For each item in the state, create a new item by feeding it the next
    // symbol it is expecting from the input, and add the new item to
    // `transitions[symbol]`. At the end of this process, for each symbol in
    // `transitions`, `transitions[symbol]` will be the kernel of the state
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

        // Get a reference to `transitions[expected_sym]`, creating a new
        // empty kernel for `expected_sym` if one does not already exist.
        OrderedMapEntry *transition;
        bool new_transition;
        if (OrderedMap_Emplace(
                &transitions, expected_sym, &transition, &new_transition)
            != BLIMP_OK)
        {
            goto error;
        }
        OrderedSet *transition_kernel = OrderedMap_GetValue(
            &transitions, transition);

        if (new_transition) {
            // If we just inserted a new kernel into `transitions`, we need
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
            goto error;
        }
        OrderedMap_CommitEmplace(&transitions, transition);
    }

    // For each (symbol, kernel) pair that we just aded to `transitions`
    // create a transition on `symbol` from this state to the state with
    // `kernel` and push it onto the depth-first traversal stack.
    const GrammarSymbol *sym;
    OrderedSet *kernel;
    OrderedMapIterator it;
    CHECK(OrderedMap_RIterator(&transitions, &it));
    while (OrderedMap_RNext(
            &transitions, &it, (const void **)&sym, (void **)&kernel))
    {
        if (Vector_PushBack(dfs_stack, &(Transition) {
                .from_state = index,
                .kernel = *kernel,
                .sym = *sym,
            }) != BLIMP_OK)
        {
            goto error;
        }
    }

    OrderedMap_Destroy(&transitions);
    return BLIMP_OK;

error:
    OrderedMap_Destroy(&transitions);
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

static inline size_t State_GetTransition(
    const State *state, const GrammarSymbol *sym)
{
    if (sym->is_terminal) {
        // Shift actions encode state transitions on non-terminals.
        Action shift = State_GetAction(state, sym->terminal);
        assert(Action_Type(shift) == SHIFT);
        return Action_Data(shift);
    } else {
        // Gotos encode transitions on non-terminals.
        return State_GetGoto(state, sym->non_terminal);
    }
}

////////////////////////////////////////////////////////////////////////////////
// The LALR State Machine
//

typedef struct {
    Vector/*<State>*/ states;
    HashMap/*<OrderedSet<Item> *, size_t>*/ index;
        // Lookup table to find the index of the unique state with a given
        // kernel.
    const Grammar *grammar;
} StateMachine;

static inline State *StateMachine_Begin(const StateMachine *m)
{
    return Vector_Begin(&m->states);
}

static inline State *StateMachine_End(const StateMachine *m)
{
    return Vector_End(&m->states);
}

static inline State *StateMachine_Next(const StateMachine *m, State *it)
{
    return Vector_Next(&m->states, it);
}

static void StateMachine_Destroy(StateMachine *m)
{
    Vector_Destroy(&m->states);
    HashMap_Destroy(&m->index);
}

static inline size_t StateMachine_NumStates(const StateMachine *m)
{
    return Vector_Length(&m->states);
}

static inline State *StateMachine_GetState(const StateMachine *m, size_t i)
{
    return Vector_Index(&m->states, i);
}

static Status StateMachine_NewState(
    StateMachine *m,
    OrderedSet *kernel,
    Vector/*<Transition>*/ *dfs_stack,
    size_t *index)
{
    // For the caller's convenience, `index` is optional. For our
    // convenience, we will point it at a local variable if it is NULL so we
    // don't have to do any more NULL checks.
    size_t local_index;
    if (index == NULL) {
        index = &local_index;
    }
    *index = Vector_Length(&m->states);

    // Append and initialize the new state.
    State *state;
    TRY(Vector_EmplaceBack(&m->states, (void **)&state));
    TRY(State_Init(state, m->grammar, kernel));

    // Update the kernel-to-state lookup table.
    assert(HashMap_Find(&m->index, &state->kernel) == NULL);
    TRY(HashMap_Update(&m->index, &state->kernel, index));

    // Push transitions from this state onto the depth-first traversal stack so
    // we visit them next.
    TRY(State_VisitTransitions(state, *index, dfs_stack));

    return BLIMP_OK;
}

static Status MakeStateMachine(const Grammar *grammar, StateMachine *m)
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
    m->grammar = grammar;

    // In order to ensure that states are added in the correct precedence order
    // for the dynamic parsing algorithm to work, we must traverse the graph
    // depth-first from the start state as we build the graph.
    //
    // We will use `stack` to keep track of states whose neighbors we have yet
    // to visit. At each state, we will push all of its unvisited neighbors onto
    // `stack`, in order of decreasing precedence, so that the lowest-precedence
    // neighbor of the current state becomes the new top of the stack. This way,
    // the next state we visit will be the neighbor of the current state with
    // the lowest precedence, giving us the weighted depth-first traversal we
    // want.
    Vector/*<Transition>*/ stack;
    Vector_Init(blimp, &stack, sizeof(Transition), NULL);

    // Create the starting state from the grammar's initial production.
    OrderedSet initial_kernel;
    OrderedSet_Init(
        blimp, &initial_kernel, sizeof(Item), (CmpFunc)Item_Cmp);
    if (OrderedSet_Insert(
            &initial_kernel, &(Item){Grammar_GetProduction(grammar, 0), 0})
        != BLIMP_OK)
    {
        goto error;
    }
    if (StateMachine_NewState(m, &initial_kernel, &stack, NULL) != BLIMP_OK) {
        goto error;
    }

    // Begin the depth-first traversal.
    while (!Vector_Empty(&stack)) {
        Transition transition;
        Vector_PopBack(&stack, &transition);

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
            // cause the new state's neighbors to be pushed onto the top of
            // `stack`, preparing us to continue the depth-first traversal in
            // the next iteration of this loop).
            if (StateMachine_NewState(
                    m, kernel, &stack, &to_index) != BLIMP_OK)
            {
                goto error;
            }
        }

        // Note the transition in the parse table row for the source state.
        State *from = StateMachine_GetState(m, transition.from_state);
        if (sym->is_terminal) {
            // Transitions on terminals are represented as shift actions.
            if (SetAction(
                    grammar, from->actions, sym->terminal, SHIFT, to_index)
                != BLIMP_OK)
            {
                goto error;
            }
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

    Vector_Destroy(&stack);
    return BLIMP_OK;

error:
    Vector_Destroy(&stack);
    StateMachine_Destroy(m);
    return Reraise(blimp);
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
    size_t from_state;
    GrammarSymbol symbol;
} ExtendedGrammarSymbol;

static bool ExtendedGrammarSymbol_Eq(
    const ExtendedGrammarSymbol *nt1,
    const ExtendedGrammarSymbol *nt2,
    void *arg)
{
    (void)arg;
    return nt1->from_state == nt2->from_state
        && GrammarSymbol_Eq(&nt1->symbol, &nt2->symbol, NULL);
}

static size_t ExtendedGrammarSymbol_Hash(
    const ExtendedGrammarSymbol *nt, void *arg)
{
    (void)arg;

    size_t hash = HASH_SEED;
    Hash_AddInteger(&hash, nt->from_state);
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
            sym->terminal = Vector_Length(&grammar->terminals);

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
    size_t from_state,
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
    size_t from_state,
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
    return Grammar_FollowSets(&grammar->projection, follows);
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
    // 0 is fine.
    if (ExtendedGrammar_ProjectTerminal(
            extended,
            0,
            grammar->eof_terminal,
            &extended->projection.eof_terminal
        ) != BLIMP_OK)
    {
        goto error;
    }

    // We derive an extended production from each item in each state in the
    // state machine where the cursor is at the start of the item.
    for (size_t state_index = 0;
         state_index < StateMachine_NumStates(m);
         ++state_index)
    {
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
            size_t curr = state_index;
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

                curr = State_GetTransition(
                    StateMachine_GetState(m, curr), sym);
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
    FILE *file, const Grammar *grammar, const ExtendedGrammar *extended)
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
        fprintf(file, "%s[%zu] ->", nt_name, nt->from_state);

        for (size_t i = 0; i < (*rule)->num_symbols; ++i) {
            const ExtendedGrammarSymbol *sym = ExtendedGrammar_Unproject(
                extended, &(*rule)->symbols[i]);
            if (sym->symbol.is_terminal) {
                const char *sym_name = *(const char **)Vector_Index(
                    &grammar->terminal_strings, sym->symbol.terminal);
                fprintf(file, " %s[%zu]", sym_name, sym->from_state);
            } else {
                const char *sym_name = *(const char **)Vector_Index(
                    &grammar->non_terminal_strings, sym->symbol.non_terminal);
                fprintf(file, " %s[%zu]", sym_name, sym->from_state);
            }
        }

        fprintf(file, "\n");
    }
}

////////////////////////////////////////////////////////////////////////////////
// Parse table
//

static Status MakeParseTable(const Grammar *grammar, StateMachine *table)
{
    Blimp *blimp = Grammar_GetBlimp(grammar);

    TRY(MakeStateMachine(grammar, table));

    ExtendedGrammar extended_grammar;
    if (MakeExtendedGrammar(grammar, table, &extended_grammar) != BLIMP_OK) {
        StateMachine_Destroy(table);
        return Reraise(blimp);
    }

    Vector follows;
    if (ExtendedGrammar_FollowSets(&extended_grammar, &follows) != BLIMP_OK) {
        ExtendedGrammar_Destroy(&extended_grammar);
        StateMachine_Destroy(table);
        return Reraise(blimp);
    }

    // Add an ACCEPT action on the $ terminal for each accepting state.
    for (State *state = StateMachine_Begin(table);
         state != StateMachine_End(table);
         state = StateMachine_Next(table, state))
    {
        if (State_IsAccepting(state)) {
            if (SetAction(
                    grammar, state->actions, grammar->eof_terminal, ACCEPT, 0)
                != BLIMP_OK)
            {
                goto error;
            }
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
        State *state = StateMachine_GetState(
            table, State_GetTransition(from_state, &final_sym->symbol));

        const Production *original_production =
            ExtendedGrammar_UnprojectProduction(&extended_grammar, i);
        if (original_production->non_terminal == START_SYMBOL) {
            // If this reduction would produce the start symbol, then there's no
            // need to reduce; just accept the input.
            assert(State_GetAction(state, grammar->eof_terminal) == ACCEPT);

            // Follow(S) should be {$}.
            assert(HashSet_Size(follow) == 1
                && HashSet_Contains(follow, &grammar->eof_terminal));

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
            if (SetAction(
                    grammar,
                    state->actions,
                    terminal->symbol.terminal,
                    REDUCE,
                    (size_t)original_production
                ) != BLIMP_OK)
            {
                goto error;
            }
        }
    }

    ExtendedGrammar_Destroy(&extended_grammar);
    Vector_Destroy(&follows);
    return BLIMP_OK;

error:
    ExtendedGrammar_Destroy(&extended_grammar);
    Vector_Destroy(&follows);
    return Reraise(blimp);
}

static void DumpParseTable(
    FILE *file, const Grammar *grammar, const StateMachine *m)
{
    size_t num_terminals = Grammar_NumTerminals(grammar);
    size_t num_non_terminals = Grammar_NumNonTerminals(grammar);

    fprintf(file, "State |");
    for (Terminal i = 0; i < num_terminals; ++i) {
        const char *string = *(const char **)Vector_Index(
            &grammar->terminal_strings, i);
        fprintf(file, " %-7.7s |", string);
    }
    fprintf(file, "|");
    for (NonTerminal i = 0; i < num_non_terminals; ++i) {
        const char *string = *(const char **)Vector_Index(
            &grammar->non_terminal_strings, i);
        fprintf(file, " %-7.7s |", string);
    }
    fprintf(file, "\n");

    for (size_t i = 0; i < StateMachine_NumStates(m); ++i) {
        const State *state = StateMachine_GetState(m, i);
        fprintf(file, "%-5zu |", i);
        for (Terminal t = 0; t < num_terminals; ++t) {
            Action action = state->actions[t];
            switch (Action_Type(action)) {
                case SHIFT:
                    fprintf(file, "   s%.2zu   |", Action_Data(action));
                    break;
                case REDUCE:
                    fprintf(file, "   r%.2zu   |",
                        ((Production *)Action_Data(action))->index);
                    break;
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

void Grammar_DumpVitals(FILE *file, const Grammar *grammar)
{
    StateMachine table;
    ExtendedGrammar extended;

    if (MakeStateMachine(grammar, &table) == BLIMP_OK &&
        MakeExtendedGrammar(grammar, &table, &extended) == BLIMP_OK)
    {
        DumpExtendedGrammar(file, grammar, &extended);
    }
    ExtendedGrammar_Destroy(&extended);
    StateMachine_Destroy(&table);

    if (MakeParseTable(grammar, &table) == BLIMP_OK) {
        DumpParseTable(file, grammar, &table);
    }
    StateMachine_Destroy(&table);
}

////////////////////////////////////////////////////////////////////////////////
// Parsing
//

Status Parse(Lexer *lex, Grammar *grammar, void *parser_state, Expr **result)
{
    Blimp *blimp = lex->blimp;

    StateMachine m;
    TRY(MakeParseTable(grammar, &m));

    // `stack` is a stack of parser state indices which is modified by SHIFT and
    // REDUCE actions.
    Vector stack;
    Vector_Init(blimp, &stack, sizeof(size_t), NULL);
    size_t start_state = 0;
    if (Vector_PushBack(&stack, &start_state) != BLIMP_OK) {
        goto error;
    }

    // `tree` is a stack of the parse tree fragments we have parsed so far.
    // Reductions pop some number of partial trees, reduce them into a single
    // larger tree, and push that back onto `tree`. At the end of parsing (when
    // we reach an ACCEPT state), `tree` should contain just a single
    // expression, which is the result of parsing.
    Vector tree;
    Vector_Init(blimp, &tree, sizeof(Expr *), ExprDestructor);

    // `output` is a stack of grammar symbols which mirrors `tree`. While `tree`
    // tells us which expressions we have generated so far, `output` tells us
    // which non-terminals (and, occasionally, terminals) they correspond to in
    // the grammar.
    Vector/*<GrammarSymbol>*/ output;
    Vector_Init(blimp, &output, sizeof(GrammarSymbol), NULL);

    // `output_precedences` is a multiset of the symbols in `output`, ordered by
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
            &m, *(size_t *)Vector_RIndex(&stack, 0));

        Token tok;
        if (Lexer_Peek(lex, &tok) != BLIMP_OK) {
            goto error;
        }

        Action action = state->actions[tok.type];
        switch (Action_Type(action)) {
            case SHIFT: {
                CHECK(Lexer_Next(lex, &tok));
                    // Consume the token we peeked.

                // Push the next state onto the stack.
                size_t next_state = Action_Data(action);
                if (Vector_PushBack(&stack, &next_state) != BLIMP_OK) {
                    goto error;
                }

                // Push the token onto the parse tree stack.
                Expr *expr;
                if (BlimpExpr_NewToken(blimp, &tok, &expr) != BLIMP_OK) {
                    goto error;
                }
                BlimpExpr_SetSourceRange(expr, &tok.range);
                if (Vector_PushBack(&tree, &expr) != BLIMP_OK) {
                    Blimp_FreeExpr(expr);
                    goto error;
                }

                // Push a terminal symbol onto `output` to correspond to the
                // token expression we just pushed onto `tree`.
                GrammarSymbol sym = {
                    .is_terminal = true, .terminal = tok.type
                };
                if (Vector_PushBack(&output, &sym) != BLIMP_OK) {
                    goto error;
                }

                // Update the count of `sym` symbols in the `output_precedences`
                // multiset.
                if (OrderedMultiset_Insert(
                        &output_precedences, &sym) != BLIMP_OK)
                {
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
                    StateMachine_Destroy(&m);
                    if (MakeParseTable(grammar, &m) != BLIMP_OK) {
                        goto error;
                    }
                }

                break;
            }

            case REDUCE: {
                const Production *production =
                    (const Production *)Action_Data(action);

                // Get the parse tree fragments which this reduction will
                // consume from the parse tree stack. Note that we do not remove
                // them from the Vector until after calling the reduction
                // handler, because we are passing a pointer into the Vector's
                // memory to the handler. This avoids an allocation and copy.
                Expr **sub_exprs = Vector_RIndex(
                    &tree, production->num_symbols - 1);

                // Call the handler to get a larger parse tree.
                ParserContext ctx = {
                    .blimp = blimp,
                    .parser_state = parser_state,
                    .arg = production->handler_arg,
                    .range = &(SourceRange){
                        .start = sub_exprs[0]->range.start,
                        .end = sub_exprs[production->num_symbols - 1]->range.end,
                    }
                };
                Expr *expr;
                if (production->handler(&ctx, sub_exprs, &expr) != BLIMP_OK) {
                    goto error;
                }
                BlimpExpr_SetSourceRange(expr, ctx.range);

                // Now we can remove the consumed parse trees from the stack and
                // push the new expression in their place.
                Vector_Contract(&tree, production->num_symbols);
                CHECK(Vector_PushBack(&tree, &expr));
                    // CHECK not TRY, because we just removed at least one
                    // element from the vector, so this should not cause an
                    // allocation that could fail.

                // Remove the corresponding symbols from `output` and
                // `output_precedences` for each expression consumed from
                // `tree`.
                for (size_t i = 0; i < production->num_symbols; ++i) {
                    GrammarSymbol sym;
                    Vector_PopBack(&output, &sym);
                    OrderedMultiset_Remove(&output_precedences, &sym);
                }

                // Push a non-terminal symbol corresponding to the newly
                // produced expression onto `output`.
                GrammarSymbol sym = {
                    .is_terminal = false,
                    .non_terminal = production->non_terminal,
                };
                if (Vector_PushBack(&output, &sym) != BLIMP_OK) {
                    goto error;
                }

                // Add the symbol we just pushed to `output_precedences`.
                if (OrderedMultiset_Insert(
                        &output_precedences, &sym) != BLIMP_OK)
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
                    &m, *(size_t *)Vector_RIndex(&stack, 0));
                if (Vector_PushBack(&stack,
                        &tmp_state->gotos[production->non_terminal])
                    != BLIMP_OK)
                {
                    goto error;
                }

                // `output_precedences` has changed, so we need to check if we
                // should update the parse table.
                const GrammarSymbol *max_precedence = OrderedMultiset_Max(
                    &output_precedences);
                if (GrammarListener_ShouldUpdate(&listener, max_precedence)) {
                    StateMachine_Destroy(&m);
                    if (MakeParseTable(grammar, &m) != BLIMP_OK) {
                        goto error;
                    }
                }

                break;
            }

            case ACCEPT: {
                // Get the result from the parse tree.
                assert(tok.type == TOK_EOF);
                assert(Vector_Length(&tree) == 1);
                Vector_PopBack(&tree, result);

                Grammar_Unlisten(grammar, &listener);
                Vector_Destroy(&stack);
                Vector_Destroy(&tree);
                Vector_Destroy(&output);
                OrderedMultiset_Destroy(&output_precedences);
                StateMachine_Destroy(&m);
                return BLIMP_OK;
            }

            case ERROR: {
                ErrorFrom(blimp, tok.range,
                    UnexpectedTokenError(tok.type),
                    "unexpected %s", StringOfTokenType(tok.type));
                goto error;
            }

        }
    }

error:
    Grammar_Unlisten(grammar, &listener);
    Vector_Destroy(&stack);
    Vector_Destroy(&tree);
    Vector_Destroy(&output);
    OrderedMultiset_Destroy(&output_precedences);
    StateMachine_Destroy(&m);
    return Reraise(blimp);
}
