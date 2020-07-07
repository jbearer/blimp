#include "internal/blimp.h"
#include "internal/expr.h"
#include "internal/hash_map.h"
#include "internal/symbol.h"

static inline Blimp *GetBlimp(const Object *obj)
{
    return HashMap_GetBlimp(&obj->scope);
}

////////////////////////////////////////////////////////////////////////////////
// Scopes: just a wrapper around a hash map mapping symbols to objects
//

static inline Status Scope_Init(Blimp *blimp, Scope *scope)
{
    HashMapOptions options = HASH_MAP_DEFAULT_OPTIONS;
    options.user_data = blimp;
    options.create_empty = true;
        // Create the scope with no associated memory. This is an optimization:
        // since many bl:mp objects (especially symbols) never have values
        // stored in their scopes, it doesn't make sense to proactively allocate
        // memory until we are ready to store something in the scope.

    return HashMap_Init(
        blimp, scope,
        sizeof(Symbol *), sizeof(Ref *),
        (EqFunc)SymbolEq, (HashFunc)SymbolHash,
        &options);
}

static inline void Scope_Destroy(Scope *scope)
{
    HashMap_Destroy(scope);
}

static inline void Scope_Clear(Scope *scope)
{
    HashMap_Clear(scope);
}

// Get a reference to the value of `sym` in the given scope, or `NULL`.
static inline Ref *Scope_Lookup(const Scope *scope, const Symbol *sym)
{
    Ref **ref = HashMap_Find(scope, &sym);
    if (ref == NULL) {
        return NULL;
    } else {
        return *ref;
    }
}

static inline Status Scope_Update(Scope *scope, const Symbol *sym, Ref *ref)
{
    return HashMap_Update(scope, &sym, &ref);
}

typedef HashMapEntry *ScopeIterator;

static inline ScopeIterator Scope_Begin(Scope *scope)
{
    return HashMap_Begin(scope);
}

static inline ScopeIterator Scope_Next(Scope *scope, ScopeIterator it)
{
    return HashMap_Next(scope, it);
}

static inline ScopeIterator Scope_End(Scope *scope)
{
    return HashMap_End(scope);
}

static inline Ref *Scope_GetValue(Scope *scope, ScopeIterator it)
{
    return *(Ref **)HashMap_GetValue(scope, it);
}

////////////////////////////////////////////////////////////////////////////////
// Object pool
//
////////////////////////////////////////////////////////////////////////////////
// Motivation
//
// A typical bl:mp program can put quite a heavy load on the memory allocator,
// making lots of short-lived allocations. For example, every time a symbol
// literal expression is evaluated, it produces a new symbol object. Very often,
// that symbol is only used as the receiver of a message and then discarded, or
// else it is used as the tag of the message itself, which is frequently
// discarded as soon as the message is finished being processed. This kind of
// rapid cycling can be a pain point for many general purpose allocators (such
// as the one provided by your favorite distribution of libc).
//
// In addition, the language needs to support garbage collection, since the
// allocation of new objects (and thus their destruction as well) is not exposed
// in the abstract machine. Thus, it makes a lot of sense that we would have a
// custom allocator for objects.
//
// Once we have an allocator dedicate specifically for allocating objects, we
// can take advantage of the ways in which the problem is much more constrained
// than for a general allocator. For example, all objects are the same size.
// This fact on its own drastically simplifies the problem of writing an
// allocator, because we can permanently divide the memory owned by the
// allocator into object-sized chunks, which means we never have to search for
// an appropriately sized region of memory (since all regions are the correct
// size) and we never have to split or coalesce regions of memory.
//
// Beyond that, not only are all of our allocations object-sized, they are all
// actually objects. This means that we can save some of the cost of
// initializing new objects by not fully deinitializing old objects when they
// are returned to the free list. For example, if we happen to get an object
// that is not short-lived, then the program may have spent a fair amount of
// work initializing and allocating memory for the hash map representing that
// object's scope. When we free that object, instead of destroying its hash map,
// we can simply empty it (which is pretty cheap) and we can then reuse its
// memory the next time that object is reused. A more complete scheme might even
// try to predict when an allocation is going to see heavy use of the hash map,
// and try to serve such allocations with an object which already has a large
// hash map allocated. The current implementation does not do this.
//
////////////////////////////////////////////////////////////////////////////////
// Architecture
//
// The memory managed by the allocator is divided into large contiguous slabs,
// or batches. Each batch consists of a small header and an array of batch_size
// objects. Each batch is itself allocated from the system allocator using
// `malloc`. Allocating space for many objects at once using batches allow us to
// amortize the cost of the system allocator.
//
// When a batch is first allocated, it constains space for batch_size objects,
// but none of those objects are initialized. When an object is first allocated
// from a batch, it is fully initialized, but when it is returned to the
// allocator, it is only partially deinitialized. As discussed above, this
// allows us to minimize some of the expensive work of initalization, such as
// allocating memory for hash maps. This scheme does require some extra
// accounting, though: we need to know which free objects are partially
// initialized, so we don't waste time reinitializing them, and which are
// uninitialized so we can initialize them before allocating them. The
// initialized free objects are linked together in a free object list which may
// traverse objects in all the batches in the system, in an arbitrary order. The
// uninitialized objects are handled per batch: each batch allocates
// uninitialized objects in order, starting from the first object in its array
// of objects, and it keeps track of the offset of the first object in its
// array.
//
// Finally, all the batches themselves are linked together in another list, in
// the order in which they were allocated from the system allocator, with the
// most recently allocated batch first. Since we only allocate a new batch when
// there are no free objects to allocate out of the existing batches, all of the
// batches in this list except possibly the first contain only initialized
// objects; their uninitialized offsets are equal to the batch size. We refer to
// these as the saturated batches. The first batch in the list may contain
// uninitialized object which can be allocated; if it does, we call it the
// active batch. We never actually use the list of batches for allocation; if we
// want to allocate an uninitialized object we always do it out of the active
// batch, and if we want an initialized object that may come from a saturated
// batch, we access it via the free list, not the list of batches. However, the
// list of batches is used at system teardown to return each of the allocated
// batches to the system allocator.
//
//  batch list                                          free list
//   |                                                   |
//  _V____________________________       ________________|________________
// | Next: -----------------------|---->| Next: ---------|----------------|->...
// | Uninitialized:--.            |     | Uninitialized:-|--------------. |
// | Objects:________V__________  |     | Objects:_______V____________ _V |
// |   | FREE | SYM | ??? | ??? | |     |   | BLOCK | FREE | SYM | FREE | |
// |   |_|__^_|____ |_____|_____| |     |   |_______|__|___|_____|__^___| |
// |_____|__|_____________________|     |______________|____________|_____|
//       |  |__________________________________________V            |
//       V__________________________________________________________|
//
// |-------Active Batch-----------|     |-------Saturated batches-------------->
//
////////////////////////////////////////////////////////////////////////////////
// Allocating
//
// To allocate an object out of this architecture, we use a tiered algorithm:
// there are three tiers of objects from which we can allocate, each more
// expensive than the last. We try each tier in succession until we succeed in
// allocating an object.
//
// The tiers, in order of most preferred (least expensive) to least preferred
// (most expensive) are:
//  1. The free list: cheapest allocation strategy, just take an already
//     partially initialized object off the head of a linked list. We have to
//     clear, but not reinitialize, its hash map, since the map may have stale
//     data from the last time it was allocated.
//  2. The active batch: slightly more expensive, requires initializing the hash
//     map for the object's scope, but does not require a malloc.
//  3. Allocating a new batch: expensive, requires calling malloc (though note
//     that we do not have to initalize the whole batch, since we do that lazily
//     when we allocate from the batch in tier 2). Done very rarely because of
//     batching.
//
////////////////////////////////////////////////////////////////////////////////
// Deallocating
//
// Deallocating is very simple: we put the object to be deallocated as is on the
// front of the free list. We do not have to do any deinitialization, such as
// emptying out the scope, since that is performed lazily if and when that
// object is later reallocated from tier 1.
//
// Note that we never return memory to the system allocator until the entire
// ObjectPool is torn down -- even if a batch becomes completely free at some
// point during its life.
//
////////////////////////////////////////////////////////////////////////////////
// Garbage collection
//
// Of course, to make use of our nice and simple deallocation algorithm, we have
// to know when to deallocate an object. Since bl:mp does not expose a detailed
// memory model to the user (objects are allocated implicitly as needed, and
// there is no explicit "free" operation) it is impossible for the user to tell
// us when an object can safely be deallocated. Therefore, we need some form of
// garbage collection.
//
// In fact, we have three independent garbage collection algorithms, each of
// which can collect different kind of garbage at varying performance cost.
// Roughly from lowest-overhead and least effective to most expensive and most
// effective, they are:
//  * Reference counting (gc-refcount)
//      Collects objects as soon as they are no longer referenced by other
//      objects or API users. Cannot collect circular references.
//  * Enhanced reference counting (gc-cycle-detection)
//      Collects certain "clumps" of objects which may all mutually reference
//      each other in a cyclic fashion, as soon as there are no more references
//      to the clump from outside the clump. Cycle detection is currently
//      limited to `parent` pointers: ERC can only collect clumps where removing
//      all the parent pointers would leave an acyclic data structure.
//  * Mark-sweep (gc-tracing)
//      Collects all unreachable objects, periodically when more memory is
//      needed, by sweeping the entire heap looking for unreachable objects.
//
// Each of these garbage collection algorithms is described in detail below.
//
////////////////////////////////////////////////////////////////////////////////
// Garbage collection: reference counting
//
// Every allocated object has two reference counts:
//  1. The internal reference count, which counts references between objects
//     within the heap. These are references through scopes and parent
//     references.
//  2. The transient reference count, which tracks how many temporary variables
//     in client code reference that object. For example, while a message send
//     is being processed by Blimp_Eval(), the Blimp_Eval() scope owns a
//     transient reference to the message and the receiver.
//
// The only reason to split the reference counts into two categories is to
// facilitate tracing garbage collection (described below): an object is
// considered _referenced_ if either reference count is nonzero, but an object
// is considered _reachable_ only if its transient reference count is nonzero,
// or if it is referenced by a reachable object.
//
// For the purposes of reference counting itself, it is useful to think of a
// single conceptual reference count for each object, which is represented by
// the sum of its internal reference count and its transient reference count.
// The object can be deallocated by reference counting if and only if its
// unified reference count goes to 0. This unified reference count is referred
// to in the remainder of this section.
//
// The reference count for an object must at all times accurately reflect the
// number of references to that object. Thus, whenever a new reference to an
// object is created (for example, an object is stored in the scope of another
// object) the reference count of the object being referenced must be
// incremented. InternalBorrow() performs this operation for internal
// references. For transient references, it is the user's responsibility to call
// BlimpObject_Borrow(), which increments the object's transient reference
// count.
//
// Whenever a reference to an object dies (for example, an object which was
// referenced in the scope of another object is replaced) its reference count
// must be decremented. The decrement is performed by InternalRelease() or
// BlimpObject_Release(). If, after decrementing the reference count of an
// object, that object's reference count is now 0, then the object is freed, and
// any references it has to other objects are released. This may in turn lead to
// other objects being freed.
//
// Using reference counting for garbage collection has some advantages:
//  * It is easy to implement and understand.
//  * It is low-overhead, allowing high throughput.
//  * It is less prone to long, unpredictable pauses than tracing GC, allowing
//    low latency.
//  * It is responsive, freeing objects immediately after they are no longer in
//    use.
//  * It is a convenient way to expose an object ownership API, using the
//    functions BlimpObject_Borrow() and BlimpObject_Release().
//
// On top of these advantages, reference counting should be sufficient for many
// kinds of programs to run without exhausting the heap. However, in the general
// case, reference counting is not a complete form of garbage collection,
// because it cannot detect cycles in the heap. Programs that create circular
// references and then leak them may run out of memory after some time.
//
////////////////////////////////////////////////////////////////////////////////
// Garbage collection: enhanced reference counting
//
// While basic reference counting sounds good (and it is, for certain kinds of
// problems) it is of limited use in bl:mp because of implicit cycles: every
// bl:mp object has a reference to its parent object which lasts for the
// duration of the child object. These implicit parent references can form
// cycles with otherwise-acyclic explicit references created by the user. We
// would like to be able to detect these kinds of "simple" cyces (cycles which
// are only circular because of parent references) using something less heavy-
// weight than tracing GC.
//
// Enhanced reference counting, or ERC, attempts to collect many cases of simple
// cycles. Before describing how the algorithm works, let's look at a very
// simple, yet very common, motivating example: a humble local variable. Here's
// some bl:mp code:
//
//      {scope|
//          x{:=|y}
//      }{.eval|.}
//
// After evaluating this piece of code, it is quite clear that the `scope`
// object has a reference to a symbol object `y`. That is, after all, the sole
// observable effect of this program. Less obvious is that the object `y` has a
// reference to the would-be temporary `:=` object, via its parent pointer.
// Likewise, that `:=` object has a reference to the `scope` object via _its_
// parent pointer, so the chain of references looks like this:
//
//                             ---------
//                             |       |
//                        .<---| scope |
//                        |    |   1   |
//                        |    ----^----
//                        |        |
//                        |        | parent
//                        |        |
//                        |    ---------
//                        |    |       |
//    scope (through `x`) |    |  :=   |
//                        |    |   1   |
//                        |    ----^----
//                        |        |
//                        |        | parent
//                        |        |
//                        |    ---------
//                        V    |       |
//                        .---->   y   |
//                             |   1   |
//                             ----^----
//
//
// Each object has been annotated with its reference count. Note that even
// though every object has a nonzero reference count of 1, all of the references
// are internal to this group of objects; none of the objects are reachable
// externally. Therefore, we could in theory free all of these objects together,
// knowing that any reference to an object being freed originates from another
// object in the group which is being freed at the same time.
//
// This is the key observation of ERC: if two objects are part of the same
// reference cycle, then if either object is ever freed, the other object must
// be freed at the same time. Therefore, both of these objects can share the
// same reference count, and that reference count does not have to account for
// references between the two objects.
//
// If two objects are part of the same cycle, we say that they are "entangled",
// because, analagous to quantumly entangled particles, if one object is freed,
// the other most also be freed simultaneously. A group of entangled objects
// which must all be freed at the same time is called a "clump". Every clump has
// a "representative", which is an object in the clump storing information about
// the clump as a whole.
//
// The most important piece of information kept by the representative is the
// clump reference count, which counts the number of references to all objects
// in the clump from objects outside of the clump. Whenever a reference is
// created between two objects which are not in the same clump, the reference
// count of the clump containing the newly referenced object is incremented.
// Whenever such a reference is lost, the clump reference count is decremented.
// If the clump reference count reaches 0, we free every object in the clump at
// once, even if the individual objects have a nonzero reference count, because
// we know all references to objects in the clump originate from other objects
// in the clump.
//
// In order to be able to free all objects in the clump, each clump maintains a
// list of all of the objects inside it. This is a doubly linked circular list.
// It is circular so that we have easy access to the last element in the clump
// given the first element. This makes appending two such lists efficient, which
// is important when entangling clumps (see below). The list is doubly linked so
// that we can efficiently remove elements. This happens when an object in a
// clump is freed by a different, more precise GC algorithm (such as mark-sweep)
// which has determined that it can free that object without freeing the rest of
// the clump. In that case, since the clump is still live, we need to remove the
// dead object from somewhere in the middle of its list. Note that we can be
// sure that no `entangled` pointer points to objects being freed from within a
// clump, because if it did, a parent pointer would also point to that object,
// and so it would still be reachable (see the clump invariants below). So we
// only need to worry about the list of objects in the clump becoming stale.
//
// Since ERC is mainly interested in detecting simple cycles, clump formation is
// oriented around the inverted tree formed by parent pointers. Each clump
// represents a contiguous fragment of the tree. For example, the boxed clump:
//
//                  *                                                         //
//        --------/-  \                                                       //
//        |     *  |   *                                                      //
//        |    / \ |                                                          //
//        |   *   *|                                                          //
//        |  /     |                                                          //
//        | *      |                                                          //
//        ----------                                                          //
//
// is allowed, because it consists of an entire subtree. This clump:
//
//                 *                                                          //
//         ------/-- \                                                        //
//         |    *  |  *                                                       //
//         |   / \ |                                                          //
//         |  *   *|                                                          //
//         --/------                                                          //
//          *                                                                 //
//
// is also allowed. Even though it doesn't contain an entire subtree, it
// contains a contiguous fragment of the tree.
//
// This clump:
//
//             -----------
//             |    *    |
//             |  /   \  |
//             | *     * |
//             -/-\-------
//             *   *
//         ---/-
//         | * |
//         -----
//
// is not allowed, because it is not contiguous.
//
// We can summarize the constraints on the clump with a few invariants:
//  * The clump representative is reachable from every object in the clump by
//    following `entangled` pointers (that is, the clump is structured
//    internally like an inverted tree).
//  * For every `entangled` pointer from A to B, B is also reachable from A by
//    following one or more `parent` pointers (that is, entanglement
//    relationships have the same shape as parent relationships). Importantly,
//    this guarantees that B will not be freed while A is still alive by some
//    other GC algorithm, since the parent pointers are reference counted.
//  * For every `entangled` pointer from A to B, every object C such that C is
//    reachable from A and B is reachable from C via parent pointers, C is in
//    the same clump as A and B (the contiguous tree fragment invariant).
//
// Technically, every object is entangled from the moment it is created with
// itself; it is the representative of a clump of size 1. We call these clumps
// "trivial", since they don't really behave any differently from individual
// objects. The interesting stuff happens when we entangle two existing clumps
// to form a larger, non-trivial clump. This happens whenever we detect that a
// cycle is being formed.
//
// Since ERC is mainly interested in detecting simple cycles (like the local
// variable example above) the cycle detection algorithm is very simple:
// whenever an edge is added between objects, check if the source object is
// reachable from the destination object via parent pointers. If it is, the new
// edge completes a simple cycle, so entangle the source and destination
// objects.
//
// The only sure way know if one object is reachable from another via parent
// pointers is to follow parent pointers until reaching either the target object
// or NULL. However, there is a heuristic which can be used to eliminate this
// possibly expensive traversal in most cases. Each object contains a Bloom
// filter representing the set of objects which it can reach via parent
// pointers. This set is static over the duration of the object's lifetime, so
// can easily be represented using a Bloom filter. Before traversing parent
// pointers, we first check if the target object is in the source object's Bloom
// filter (a simple bitwise and operation). If it is, then the target might be
// reachable from the source via parent pointers, and we need to do the
// traversal to be sure. But if it is not, then the target is definitely not
// reachable, so we don't need to do the traversal.
//
// The cycle detection tells us when to entangle two clumps. The last missing
// piece of the ERC algorithm is _how_ to entangle two clumps. Suppose we have
// the following object structure:
//
//    -----
//    | A |
//    --^--
//      |
//    -----
//    | B |
//    --^--
//      |
//    -----
//    | C |
//    -----
//
// where the edges between objects are parent pointers. Note that A, B, and C
// may each be the representative of a large existing clump, or they may be
// singleton clumps. As long as we are careful to always deal with
// representatives, it doesn't matter, so other members of existing clumps have
// been omitted from the diagram.
//
// Let's say we want to entangle C with A, and they are not already entangled.
// We first check C's parent pointer. It is B, not A, and by the contiguous
// fragment invariant, we already know we will need to entangle B with A before
// we can entangle C. We check if B is already entangled with A. Since it is not
// we proceed to entangle B with A recursively.
//
// We check B's parent pointer. It is A, and so we've reached the base case of
// the recursion. To entangle these to clumps, we point B's `entangled` pointer
// at A, which immediately makes B a member of clump A. We also need to update
// the clump reference count. To do this, we add the old clump reference count
// of B to the clump reference count of A, and subtract 1. The addition accounts
// for external references to the objects which were in B's clump, and which are
// now in A. The subtraction accounts for the parent pointer of B, which was a
// pointer from outside of clump A into clump A, and thus was counted, but which
// is now internal to the clump, and thus should no longer be counted.
//
// After entangling B with A, we pop one frame off the stack of recursion. We
// are once again trying to entangle C with A. We see that C's parent pointer is
// B, and B is entangled with A, so all we have to do is entangle C with B. We
// do that using the same algorithm for entangling an object with its parent
// that we used to entangle B with A.
//
// It is useful at this point to walk through a few concrete examples of the
// ERC algorithm in action, to see more clearly how it works and as evidence
// that it is actually able to free some simple cycles.
//
// ERC Example 1: Linear tree
//
// Let's first consider the simple case where the parent tree has no branches.
// We'll consider a data structure like the A-B-C chain above, where we have
// transient references to A and C and we are creating a scope reference from
// A to C:
//
//              |                       |
//              |                       | transient
//              |                       |
//              |                   ----V----
//              |                   |       |
//              |                   |   A   |
//              |                   |  2,2  |
//              |                   ----^----
//              |                       |
//              |                       | parent
//              |                       |
//    transient |                   ---------
//              |                   |       |
//              |                   |   B   |
//              |                   |  1,1  |
//              |                   ----^----
//              |                       |
//              |                       | parent
//              |                       |
//              |                   ---------
//              |                   |       |
//              |                   |   C   |
//              |                   |  1,1  |
//              |                   ----^----
//              |                       |
//              ------------------------.
//
//
// Each object has been annotated with its reference count, and each clump
// representative has been annotated with its clump reference count (initially,
// all of the objects are in trivial clumps, so they all have a clump reference
// count which is the same as their reference count).
//
//
// Now we add a scope edge reference from A to C:
//
//              |                       |
//              |                       | transient
//              |                       |
//              |                   ----V----
//              |                   |       |
//              |              .<---|   A   <-----------.
//              |              |    |  2,2  |           ^
//              |              |    ----^----           |
//              |              |        |               |
//              |              |        | parent        |
//              |              |        |               |
//    transient |              |    ---------           |
//              |              |    |       |           |
//              |    scope     |    |   B   |-----------^ entangled
//              |              |    |   1   |           |
//              |              |    ----^----           |
//              |              |        |               |
//              |              |        | parent        |
//              |              |        |               |
//              |              |    ---------           |
//              |              V    |       |           |
//              |              .---->   C   |---------->.
//              |                   |   2   |
//              |                   ----^----
//              |                       |
//              ------------------------.
//
//
// When we create the scope edge from A to C, we determine using C's Bloom
// filter that A could be a predecessor of C (in this case, it actually is). So
// we attempt to entangle C with A. C's parent is B, not A, so first we entangle
// B with A. We increment A's clump reference count by that of B (which is 1),
// and we decrement it by 1 to account for the fact that the parent reference
// from B to A is now internal to the clump. Thus, the clump reference count of
// A remains 2. We then do the same to entangle C with A, and again the clump
// reference count of A does not change. Now A, B, and C are all entangled, and
// we have correctly identified that there are only two external references to
// this clump.
//
//
// Now let's look at what happens when we lose our external references to this
// data structure. The first transient pointer goes away:
//
//                                      |
//                                      | transient
//                                      |
//                                  ----V----
//                                  |       |
//                             .<---|   A   <-----------.
//                             |    |  2,1  |           ^
//                             |    ----^----           |
//                             |        |               |
//                             |        | parent (r)    |
//                             |        |               |
//                             |    ---------           |
//                             |    |       |           |
//                   scope (e) |    |   B   |-----------^ entangled
//                             |    |   1   |           |
//                             |    ----^----           |
//                             |        |               |
//                             |        | parent (r)    |
//                             |        |               |
//                             |    ---------           |
//                             V    |       |           |
//                             .---->   C   |---------->.
//                                  |   1   |
//                                  ---------
//
// We've deceremented the reference count of C, as well as that of its
// entanglement group, currently represented by A.
//
// Second transient pointer goes away:
//
//                                  ---------
//                                  |       |
//                             .<---|   A   <-----------.
//                             |    |  1,0  |           ^
//                             |    ----^----           |
//                             |        |               |
//                             |        | parent (r)    |
//                             |        |               |
//                             |    ---------           |
//                             |    |       |           |
//                   scope (e) |    |   B   |-----------^ entangled
//                             |    |   1   |           |
//                             |    ----^----           |
//                             |        |               |
//                             |        | parent (r)    |
//                             |        |               |
//                             |    ---------           |
//                             V    |       |           |
//                             .---->   C   |---------->.
//                                  |   1   |
//                                  ---------
//
// We've decremented the reference count of A, a well as the reference count of
// the entanglement group. Notice that none of the objects have a reference
// count of 0: they all of one internal reference to them (either a scope
// reference or a parent reference). However, the reference count of the group
// _is_ 0, which indicates that all the references to objects in the group are
// from other objects in the group. Therefore, we can safely free the entire
// entanglement group.
//
// Now let's look at a slightly more complicated example, where the data
// structure is a tree with parallel branches. This example will require us to
// merge nontrivial clumps.
//
//              |                    |                   |
//              |                    | transient         |
//              |                    |                   |
//              |                ----V----               |
//              |                |       |               |
//              |                |   A   |               |
//              |                |  2,2  |               |
//              |                ----^----               |
//              |                    |                   |
//              |                    | parent            |
//    transient |                    |                   | transient
//              |                ---------               |
//              |                |       |               |
//              |                |   B   |               |
//              |                |  2,2  |               |
//              |                ---,-.---               |
//              |       parent    ,`   `.   parent       |
//              |               ,`       `.              |
//              |             ,`           `.            |
//              |         ---------     ---------        |
//              |         |       |     |       |        |
//              ---------->   C   |     |   D   <---------
//                        |  1,1  |     |  1,1  |
//                        ---------     ---------
//
// We create a scope edge from A to C:
//
//              |                    |                   |
//              |                    | transient         |
//              |                    |                   |
//              |                ----V----               |
//              |                |       |               |
//              |   ,------------|   A   |               |
//              |  |             |  2,3  |               |
//              |  |             ----^----               |
//              |  |                 |                   |
//              |  |                 | parent            |
//    transient |  |                 |                   | transient
//              |  |             ---------               |
//              |  | scope       |       |               |
//              |  |             |   B   |               |
//              |  |             |   2   |               |
//              |  |             ---,-.---               |
//              |  |    parent    ,`   `.   parent       |
//              |  |            ,`       `.              |
//              |  |          ,`           `.            |
//              |  |      ---------     ---------        |
//              |  `------>       |     |       |        |
//              |         |   C   |     |   D   <---------
//              `--------->   2   |     |  1,1  |
//                        ---------     ---------
//
// This increments the refcount of C. It also causes C, B, and A to become
// entangled. The refcount of the entanglement group is 3: the two transient
// references to A and C, and the parent reference from D to B. The new scope
// reference, and the parent references  from C to B and from B to A, are not
// counted, because they are internal to the group. So far, this is just like
// the previous example, except the reference count for the clump is one higher,
// since the parent pointer from D to B is an additional external reference.
//
// Next, we create a scope edge from A to D:
//
//              |                    |                   |
//              |                    | transient         |
//              |                    |                   |
//              |                ----V----               |
//              |                |       |               |
//              |   ,------------|   A   |------------.  |
//              |  |             |  2,3  |            |  |
//              |  |             ----^----            |  |
//              |  |                 |                |  |
//              |  |                 | parent         |  |
//    transient |  |                 |                |  | transient
//              |  |             ---------            |  |
//              |  | scope       |       |      scope |  |
//              |  |             |   B   |            |  |
//              |  |             |   2   |            |  |
//              |  |             ---,-.---            |  |
//              |  |    parent    ,`   `.   parent    |  |
//              |  |            ,`       `.           |  |
//              |  |          ,`           `.         |  |
//              |  |      ---------     ---------     |  |
//              |  `------>       |     |       <-----.  |
//              |         |   C   |     |   D   |        |
//              `--------->   2   |     |   2   <---------
//                        ---------     ---------
//
// This increments the refcount of D. It also causes D to become entangled with
// A (via B, which is already entangled with A). This causes the group refcount
// to be decremented once (for the parent pointer from D to B, since this
// reference was counted but has now become internal to the group) and
// incremented once (for the old groupo refcount of D) leaving the group
// refcount unchanged overall.
//
// The group refcount of 3 now accurately reflects the number of external
// references to the group: we have three transient references, and all other
// references are internal. Since the group refcount is accurate, we can see
// that releasing the three transient references should cause the entire group
// to be freed.
//
////////////////////////////////////////////////////////////////////////////////
// Garbage collection: mark-sweep
//
// While reference counting and ERC have their advantages for the kinds of
// garbage which they are able to collect, the fact is that there are many kinds
// of data structures which those techniques cannot collect, and so many real
// programs will eventually need some form of garbage collection which is
// capable of relibably freeing all of their unused objects, in order to avoid
// memory leaks.
//
// The good news is that this most relivable GC algorithm does not have to be
// particularly efficient. Since reference counting and ERC are continuously
// freeing many common kinds of garbge, the rate at which uncollected garbage
// accumulates in many programs will be far slower than it would be in a system
// that only used a tracing garbage collector. Every so often when we do run
// the general garbage collector, we can afford to sweep the entire heap looking
// for garbage, and that's exactly what the tracing garbage collector does.
//
// The tracing GC is a very simple mark-sweep algorithm which frees all objects
// not reachable from a root. A "root" is defined as any object with a nonzero
// transient reference count; those are the objects that the program on top of
// us is currently using. Note that the global object is always a root, because
// the interpreter itself owns a transient reference to this object, which lasts
// for the entire lifetime of the interpreter.
//
// To collect garbage, we first sweep the entire heap searching for roots. Once
// we find the roots, we make a depth-first traversal of the heap starting from
// those roots and following parent pointers and scope references. Each object
// we encounter during this traversal gets marked "reached". Finally, we sweep
// the heap once more freeing all allocated objects which were not reached.
//
// The garbage collector can be started explicitly by calling
// Blimp_CollectGarbage(). By default, it also runs automatically whenever we
// need a new object, we don't have any free ones available, and we have
// allocated `gc_batches_per_collection` since the last collection.
//

typedef struct ObjectBatch {
    size_t uninitialized;
    struct ObjectBatch *next;
    Object objects[];
} ObjectBatch;

typedef struct RefBatch {
    size_t uninitialized;
    struct RefBatch *next;
    Ref refs[];
} RefBatch;

Status ObjectPool_Init(Blimp *blimp, ObjectPool *pool)
{
    TRY(Blimp_GetSymbol(blimp, "symbol", &pool->symbol_tag));

    // Compute batch size in terms of number of objects, rather than bytes, and
    // round up to ensure we get at least one object per batch.
    pool->batch_size = (blimp->options.gc_batch_size/sizeof(Object)) + 1;

    TRY(Malloc(
        blimp,
        sizeof(ObjectBatch) + sizeof(Object)*pool->batch_size,
        &pool->batches
    ));
    pool->batches->uninitialized = 0;
    pool->batches->next = NULL;
    pool->free_list = NULL;
    pool->batches_since_last_gc = 1;
    pool->gc_collections = 0;
    pool->seq = 0;
    pool->free_refs = NULL;
    pool->refs = NULL;

    Random_Init(&pool->random, 42);

    return BLIMP_OK;
}

void ObjectPool_Destroy(Blimp *blimp, ObjectPool *pool)
{
    // Free any remaining allocated objects.
    for (ObjectBatch *batch = pool->batches; batch; batch = batch->next) {
        for (size_t i = 0; i < batch->uninitialized; ++i) {
            Object *obj = &batch->objects[i];

            if (obj->type == OBJ_BLOCK) {
                Blimp_FreeExpr(obj->code);
            }

            Scope_Destroy(&obj->scope);
        }
    }

    // Free object batches.
    ObjectBatch *batch = pool->batches;
    while (batch) {
        ObjectBatch *next = batch->next;
        Free(blimp, &batch);
        batch = next;
    }

    // Free ref batches.
    RefBatch *ref_batch = pool->refs;
    while (ref_batch) {
        RefBatch *next = ref_batch->next;
        Free(blimp, &ref_batch);
        ref_batch = next;
    }
}

static Status AllocRef(Blimp *blimp, Ref **ref)
{
    ObjectPool *pool = &blimp->objects;

    if (pool->free_refs == NULL) {
        // If the free list is empty, try to allocate from a partial batch.
        if (pool->refs == NULL ||
            pool->refs->uninitialized >= blimp->options.gc_batch_size)
        {
            // If the first batch is fully allocated, allocate a new batch.
            RefBatch *batch;
            TRY(Malloc(
                blimp,
                sizeof(RefBatch) + blimp->options.gc_batch_size*sizeof(Ref),
                &batch));

            batch->uninitialized = 0;
            batch->next = pool->refs;
            pool->refs = batch;
        }

        assert(pool->refs != NULL);
        assert(pool->refs->uninitialized < blimp->options.gc_batch_size);

        pool->free_refs = &pool->refs->refs[pool->refs->uninitialized++];
        pool->free_refs->next = NULL;
        pool->free_refs->prev = NULL;
        pool->free_refs->to = NULL;
    } else {
        assert(pool->free_refs->to == NULL);
    }

    assert(pool->free_refs != NULL);
    *ref = pool->free_refs;
    pool->free_refs = (*ref)->next;

    assert((*ref)->prev == NULL);
    assert((*ref)->to == NULL);
    (*ref)->next = NULL;

    return BLIMP_OK;
}

static void FreeRef(Blimp *blimp, Ref *ref)
{
    ObjectPool *pool = &blimp->objects;

    ref->next = pool->free_refs;
    ref->prev = NULL;
    ref->to = NULL;

    pool->free_refs = ref;
}

////////////////////////////////////////////////////////////////////////////////
// Reference counting
//

static void FreeObject(Object *obj, bool recursive);

static inline void RC_BorrowRef(Object *from, Ref *ref)
{
    if (from != ref->to) {
        // Only increment the referece count if the destination object is
        // different from the source object. There's no need to count
        // references-to-self, because it is impossible for an object to be
        // destroyed before itself is destroyed.
        ++ref->to->internal_refcount;
    }
}

static inline void RC_ReleaseRef(Object *from, Ref *ref)
{
    // If `from == to`, we didn't acquire a reference in InternalBorrow(), so
    // there's nothing to release.
    if (from != ref->to) {
        assert(ref->to->internal_refcount > 0);
        if (--ref->to->internal_refcount == 0 &&
            ref->to->transient_refcount == 0)
        {
            FreeObject(ref->to, true);
        }
    }
}

static inline void RC_BorrowParent(Object *obj)
{
    assert(obj->parent != NULL);
    assert(obj->parent != obj);
    ++obj->parent->internal_refcount;
}

static inline void RC_ReleaseParent(Object *obj)
{
    assert(obj->parent != NULL);
    assert(obj->parent != obj);
    if (--obj->parent->internal_refcount == 0 &&
        obj->parent->transient_refcount == 0)
    {
        FreeObject(obj->parent, true);
    }
}

////////////////////////////////////////////////////////////////////////////////
// Enhanced reference counting
//

static inline Object *ERC_GetClump(Object *obj);
static void ERC_FreeClump(Object *clump);
static inline bool ERC_Entangle(Object *from, Object *to);
static void ERC_MergeClumps(Object *root, Object *child);

#define BLOOM_FILTER_NUM_HASHES 4
    // Number of hash functions, or number of bits set for each object in a 128-
    // bit Bloom filter. This number was chosen because it has decent empirical
    // performance on the existing ERC benchmarks. This should be investigated
    // and tuned more carefully.

// Perform one-time initialization of the ERC data in an object. This function
// is called once, the first time an object is allocated from a batch.
// Everything it does must be reusable when the object is reused.
static void ERC_StaticInit(ObjectPool *pool, Object *obj)
{
    // Give the object its own bit pattern which will be used to represent this
    // object in Bloom filters. The bit pattern should be unique with high
    // probability.
    //
    // In theory, what we want here is a good hash of the object. Set one bit in
    // the object's self_mask for each Bloom filter hash. However, since we
    // assign each object a bit pattern once and then store that pattern with
    // the object, we never need to compute a hash from an object, because we
    // can just look it up. So this function doesn't need to be deterministic;
    // it just needs to be uniform. In other words, we want the bits we set here
    // to be pseudo-random.

    obj->self_mask = BV128_ZER0;
        // Initialize the mask to all zeros.

    // Randomly set some number of bits.
    for (size_t i = 0; i < BLOOM_FILTER_NUM_HASHES; ++i) {
        obj->self_mask = bv128_Set(
            obj->self_mask, Random_NextWord(&pool->random) % 128);
    }
}

// Reinitialize the ERC data in an object. Unlike ERC_StaticInit, this function
// will be called each time the object is reused.
static void ERC_Init(Object *obj)
{
    // Make this object the representative of a trivial clump.
    obj->entangled = NULL;
    obj->clump_refcount = 0;
    obj->clump_next = obj;
    obj->clump_prev = obj;
    obj->clump_references = NULL;

    if (obj->parent && obj->parent->type != OBJ_GLOBAL) {
        // The objects predecessors set consists of its parent together with its
        // parent's predecessors. To compute this set, we union (bitwise-or) the
        // parent's predecessors Bloom filter with the parent's self_mask.
        //
        // We don't include the global object in the predecessors sets of any of
        // its children, because nothing is ever entangled with the global
        // object, so having it test positive in any Bloom filters would be
        // pointless.
        obj->predecessors = bv128_Or(
            obj->parent->predecessors, obj->parent->self_mask);
    } else {
        // If the object has no parent, it has no predecessors.
        obj->predecessors = BV128_ZER0;
    }
}

static inline void ERC_ExternalBorrow(Object *obj)
{
    ++ERC_GetClump(obj)->clump_refcount;
}

static inline void ERC_ExternalRelease(Object *obj)
{
    Object *clump = ERC_GetClump(obj);
    assert(clump->clump_refcount > 0);
    if (--clump->clump_refcount == 0) {
        ERC_FreeClump(clump);
    }
}

static inline void ERC_BorrowRef(Object *from, Ref *ref)
{
    Object *from_clump = ERC_GetClump(from);
    Object *to_clump   = ERC_GetClump(ref->to);
    if (from_clump == to_clump) {
        // The objects are already entangled, there's nothing to do.
        return;
    }

    // The objects are not yet entangled. Try to entangle them. This will fail
    // if `to_clump` is not a descendant of `from_clump`, so this reference is
    // not creating a simple cycle, or if we decide not to entangle the objects
    // as an optimization (for example, one of the clumps is getting too big).
    if (!ERC_Entangle(from_clump, to_clump)) {
        ERC_ExternalBorrow(to_clump);
            // If we did not entangle the objects, then just record the new
            // reference on the existing clump.

        // Add the reference to `from_clump`s list of outgoing references, so
        // that if we ever do entangle these two clumps, we will know to stop
        // counting this reference.
        ref->next = from_clump->clump_references;
        if (ref->next != NULL) {
            ref->next->prev = ref;
        }

        ref->prev = NULL;
        from_clump->clump_references = ref;
    } else {
        assert(ERC_GetClump(from) == ERC_GetClump(ref->to));
    }
}

static inline void ERC_ReleaseRef(Object *from, Ref *ref)
{
    Object *from_clump = ERC_GetClump(from);
    Object *to_clump   = ERC_GetClump(ref->to);

    // We need to make sure these objects belong to two different clumps,
    // because if they are entangled then references between them are not
    // counted in the clump reference count, so we can't decrement the reference
    // count here.
    if (from_clump != to_clump) {
        // Remove the reference from `from_clump`s list of outgoing references.
        if (ref->next != NULL) {
            ref->next->prev = ref->prev;
        }

        if (ref->prev != NULL) {
            assert(ref != from_clump->clump_references);
            ref->prev->next = ref->next;
        } else {
            assert(ref == from_clump->clump_references);
            from_clump->clump_references = ref->next;
        }

        ref->prev = NULL;
        ref->next = NULL;

        // Decrement the reference count.
        if (--to_clump->clump_refcount == 0) {
            ERC_FreeClump(to_clump);
        }
    } else {
        assert(ref->next == NULL);
        assert(ref->prev == NULL);
    }
}

static inline void ERC_BorrowParent(Object *obj)
{
    assert(obj->parent != NULL);

    Object *parent_clump = ERC_GetClump(obj->parent);
    Object *obj_clump    = ERC_GetClump(obj);

    if (parent_clump != obj_clump) {
        // We only want to count this reference if the object is not already
        // entangled with its parent. If they are entangled, then this is an
        // intra-clump reference, which doesn't count.

        ERC_ExternalBorrow(parent_clump);
            // Unlike scope references (which might complete a simple cycle)
            // references to parents aren't treated specially by ERC, because
            // parent pointers alone always from an acyclic tree. So once we are
            // sure the object and it's parent aren't entangled, we can just
            // treat this as an external reference.
    }
}

static inline void ERC_ReleaseParent(Object *obj)
{
    assert(obj->parent != NULL);

    Object *parent_clump = ERC_GetClump(obj->parent);
    Object *obj_clump    = ERC_GetClump(obj);

    if (parent_clump != obj_clump) {
        // When we created this reference, we only counted it if the object
        // wasn't already entangled with its parent. So we can only release it
        // under the same condition.

        ERC_ExternalRelease(parent_clump);
            // Once we know the two objects aren't entangled, parent references
            // are treated just like external references.
    }
}

static inline Object *ERC_GetClump(Object *obj)
{
    if (obj->entangled == NULL) {
        return obj;
    }

    Object *clump = ERC_GetClump(obj->entangled);

    obj->entangled = clump;
        // If we had to recursively traverse `entangled` pointers to find the
        // clump representative, update `obj`s `entangled` pointer to point
        // directly to the representative, so that the next time we try to get
        // its clump, it will be fast.
        //
        // This is similar to the path compression idea in a union-find data
        // structure, and should guarantee a small bound on the number of
        // pointer indirections needed to find the clump representative from any
        // member of the clump.

    return clump;
}

static bool ERC_EntangleRecursive(Object *from, Object *to)
{
    assert(from->entangled == NULL);
    assert(to->entangled == NULL);
    assert(from != to);

    if (to->parent == NULL) {
        // We can only entangle an object with its parent or a parent of its
        // parent, so if the object has no parent, we can't entangle it.
        return false;
    }

    Object *parent = ERC_GetClump(to->parent);

    if (parent == from) {
        // If the object we're trying to entangle with is our immediate parent,
        // we can go ahead and merge the two clumps.
        ERC_MergeClumps(from, to);
        return true;
    } else {
        // Otherwise, we first need to entangle our parent with `from`, so that
        // when we entangle with `from`, we aren't breaking the contiguous tree
        // fragment invariant of clumps. We recursively call ERC_Entangle to do
        // this entanglement.
        //
        // Note that since we haven't yet modified either clump, these recursive
        // calls will walk all the way up the tree from `to` to `from` before
        // making any modifications. So if one of the recursive calls returns
        // `false` because `from` turned out not to be reachable (due to a Bloom
        // filter false positive) then we will be able to return `false` in turn
        // without having modified the clumps.
        if (ERC_EntangleRecursive(from, parent)) {
            ERC_MergeClumps(from, to);
            return true;
        } else {
            return false;
        }
    }
}

static inline bool ERC_Entangle(Object *from, Object *to)
{
    Blimp *blimp = GetBlimp(from);

    if (from->type == OBJ_GLOBAL) {
        // Never entangle any object with the global object. Since the global
        // object is never freed, any object in its clump can never be freed by
        // ERC. So entangling an object with the global object can never help
        // us. It's better to just let objects form cycles with the global
        // object and hope that those cycles are later broken.
        return false;
    }

    if (from->seq > to->seq) {
        // Any object is younger than its ancestors. Therefore, if `from` is
        // younger than `to`, `to` cannot be a descendant of `from`, so this
        // edge is not completing a simple cycle.
        return false;
    }
    assert(from->seq < to->seq);

    if (blimp->options.gc_max_clump_size &&
        blimp->options.gc_max_clump_size + from->seq < to->seq)
    {
        // The age difference between `from` and `to` is greater than the
        // maximum configured clump size. Essentially, this means that `from` is
        // a very long-lived object compared to `to`. Therefore, entangling `to`
        // with `from` is more likely to force `to` to live longer than
        // necessary, than it is to help free it. We're better off not
        // entangling the two objects, keeping the clump sizes small, and hoping
        // that any cycles between them are broken manually by the user. If not,
        // the tracing GC will eventually get it.
        return false;
    }

    if (from != to && !bv128_Test(bv128_And(from->self_mask, to->predecessors))) {
        // If the bits corresponding to `from` are not set in the predecessors
        // Bloom filter of `to`, then `to` is definitely not a descendant of
        // `from`, and so this edge is not completing a parent pointer cycle.
        return false;
    }

    return ERC_EntangleRecursive(from, to);
}

static void ERC_MergeClumps(Object *root, Object *child)
{
    assert(root->entangled == NULL);
    assert(child->entangled == NULL);
    assert(root != child);
    assert(root->seq < child->seq);
        // `child` is a descendant of `root`.

    // Update refcounts.
    assert(root->clump_refcount > 0);
    root->clump_refcount -= 1;
    root->clump_refcount += child->clump_refcount;

    // Append the lists of objects in the clump.
    Object *last = child->clump_prev;
    root->clump_prev->clump_next = child;
    child->clump_prev = root->clump_prev;
    root->clump_prev = last;
    last->clump_next = root;

    // Entangle the child to the root.
    child->entangled = root;

    // For each outgoing reference from the child clump, if it points to a
    // descendant of the root, make sure that that descendant is entangled with
    // `root` and that the reference is not counted in the clump reference
    // count. This accounts for references between siblings of a common root.
    //
    // For example, consider the following data structure:
    //
    //                      root                                              //
    //                      /  \                                              //
    //                child1--->child2                                        //
    //
    // where `child1` and `child2` both have parent pointers to `root`, and
    // `child1` has a scope reference to `child2`. Creating that scope reference
    // would not entangle `child1` with `child2` because neither is a descendant
    // of the other.
    //
    // But say we now create a scope reference from `root` to `child1`. This
    // creates a simple cycle involving `root` and `child1`, which we detect,
    // causing us to entangle `root` with `child2`. But it also creates a cycle
    // involving `root`, `child1`, _and_ `child2`. We do not immediately detect
    // this cycle, because `child2` is not involved in the operation at all.
    //
    // In general, any cycle involving parent pointers and more than one scope
    // reference will not be detected by the normal cycle detection procedure.
    // To detect these cycles, whenever we entangle a child with one of its
    // ancestors, we need to make sure that all of the other ancestors that the
    // child references are also entangled, because they all participate in
    // cycles with the common ancestor and the child.
    //
    // Note that we only have to do this for outgoing references from `child`,
    // not from `root`. If `root` had any outgoing references to any of its
    // descendants, they would already have been accounted for using the normal
    // cycle detection algorithm.
    Ref *ref = child->clump_references;
    while (ref != NULL) {
        Ref *next = ref->next;

        Object *clump = ERC_GetClump(ref->to);
        if (root == clump ||
                // The child has an existing reference to an object in the root
                // clump. Since `child` and `root` previously represented to
                // different clumps, this reference was counted in the root
                // clump reference count, but it should no longer be since we
                // are merging the clumps.
            ERC_Entangle(root, clump)
                // The child's reference does not point directly into the root
                // clump, but it points to a descendant of `root`, which we must
                // now entangle.
        ) {
            // Discount the reference which was previously counted, now that it
            // is between two objects in the same clump.
            assert(root->clump_refcount > 0);
            --root->clump_refcount;

            ref->next = NULL;
            ref->prev = NULL;
        } else {
            // This is still an external reference to some other clump. Add it
            // to the list of outgoing references from the root so that we can
            // check it again if `root` is ever merged with one of its
            // ancestors.
            ref->next = root->clump_references;
            if (ref->next != NULL) {
                ref->next->prev = ref;
            }

            ref->prev = NULL;
            root->clump_references = ref;
        }

        ref = next;
    }

    child->clump_references = NULL;
}

static inline void ERC_RemoveFromClump(Object *obj)
{
    Object *clump = ERC_GetClump(obj);

    if (clump->clump_refcount == 0) {
        return;
    }

    obj->clump_next->clump_prev = obj->clump_prev;
    obj->clump_prev->clump_next = obj->clump_next;
}

static void ERC_FreeClump(Object *clump)
{
    assert(clump->entangled == NULL);

    Object *obj = clump;

    do {
        assert(obj->transient_refcount == 0);

        Object *next = obj->clump_next;
        FreeObject(obj, true);
        obj = next;
    } while (obj != clump);
}

////////////////////////////////////////////////////////////////////////////////
// Unified reference counting API
//

static inline void BorrowRef(Object *from, Ref *ref)
{
    Blimp *blimp = GetBlimp(from);

    if (blimp->options.gc_refcount) {
        RC_BorrowRef(from, ref);
    }
    if (blimp->options.gc_cycle_detection) {
        ERC_BorrowRef(from, ref);
    }
}

static inline void ReleaseRef(Object *from, Ref *ref)
{
    if (ref->to->type == OBJ_FREE) {
        // It's possible that we have a reference to a free object if that
        // object has already been freed by the tracing garbage collector.
        return;
    }

    Blimp *blimp = GetBlimp(from);

    if (blimp->options.gc_cycle_detection) {
        ERC_ReleaseRef(from, ref);
    }

    if (blimp->options.gc_refcount) {
        RC_ReleaseRef(from, ref);
    }
}

static inline void BorrowParent(Object *obj)
{
    Blimp *blimp = GetBlimp(obj);

    if (blimp->options.gc_refcount) {
        RC_BorrowParent(obj);
    }
    if (blimp->options.gc_cycle_detection) {
        ERC_BorrowParent(obj);
    }
}

static inline void ReleaseParent(Object *obj)
{
    if (obj->parent->type == OBJ_FREE) {
        // It's possible that we have a reference to a free object if that
        // object has already been freed by the tracing garbage collector.
        return;
    }

    Blimp *blimp = GetBlimp(obj);

    if (blimp->options.gc_cycle_detection) {
        ERC_ReleaseParent(obj);
    }
    if (blimp->options.gc_refcount) {
        RC_ReleaseParent(obj);
    }
}

////////////////////////////////////////////////////////////////////////////////
// Mark-Sweep Tracing GC
//

static void MarkReachable(ObjectPool *pool)
{
    // Mark reachable objects using a depth-first traversal starting from
    // roots (objects with a nonzero transient refcount).
    Object *stack = NULL;
        // Invariant: every object on the stack has been marked `reached`. This
        // is slightly different than many depth-first search algorithms, which
        // mark an object reached only when it is popped off the stack. By
        // checking if an object has already been reached before pushing it on
        // the stack, we ensure that each object appears on the stack at most
        // once, which is important because each object only has a single next
        // pointer for the intrusive stack.

    // Find all the roots and push them onto the stack.
    for (ObjectBatch *batch = pool->batches; batch; batch = batch->next) {
        for (size_t i = 0; i < batch->uninitialized; ++i) {
            Object *obj = &batch->objects[i];

            if (obj->transient_refcount && !obj->reached) {
                assert(obj->type != OBJ_FREE);
                obj->reached = true;
                obj->next = stack;
                stack = obj;
            }
        }
    }

    // The stack now contains all the roots. Start traversing their children.
    while (stack) {
        // Pop the next object off the stack.
        Object *obj = stack;
        stack = stack->next;

        assert(obj->type != OBJ_FREE);
            // We shouldn't be able to reach any free objects.
        assert(obj->reached);
            // Everything on the stack has been reached.

        // Push this object's children onto the stack.
        for (ScopeIterator it = Scope_Begin(&obj->scope);
             it != Scope_End(&obj->scope);
             it = Scope_Next(&obj->scope, it))
        {
            Object *child = Scope_GetValue(&obj->scope, it)->to;
            assert(child->type != OBJ_FREE);

            if (!child->reached) {
                child->reached = true;
                child->next = stack;
                stack = child;
            }
        }

        // Push this object's parent onto the stack.
        if (obj->parent && !obj->parent->reached) {
            assert(obj->parent->type != OBJ_FREE);
            obj->parent->reached = true;
            obj->parent->next = stack;
            stack = obj->parent;
        }
    }
}

#ifndef NDEBUG
static void DoHeapCheck(Blimp *blimp)
{
    MarkReachable(&blimp->objects);
        // Call MarkReachable() just for the assertions that it does.

    // Reset all the `reached` fields.
    for (ObjectBatch *batch = blimp->objects.batches;
         batch;
         batch = batch->next)
    {
        for (size_t i = 0; i < batch->uninitialized; ++i) {
            batch->objects[i].reached = false;
        }
    }

    // Check that the free references list is not circular.
    for (Ref *ref = blimp->objects.free_refs; ref; ref = ref->next) {
        assert(ref->to == NULL);
        assert(ref->prev == NULL);
        ref->prev = ref;
    }
    for (Ref *ref = blimp->objects.free_refs; ref; ref = ref->next) {
        ref->prev = NULL;
    }
}
#endif

static inline void HeapCheck(Blimp *blimp)
{
    (void)blimp;

#ifndef NDEBUG
    if (!blimp->options.gc_heap_check) {
        return;
    }

    DoHeapCheck(blimp);
#endif
}

void ObjectPool_CollectGarbage(ObjectPool *pool)
{
    MarkReachable(pool);

    // Traverse the entire heap, resetting the reachable flags and freeing
    // allocated objects which were not marked as reachable and are not
    // transiently referenced.
    for (ObjectBatch *batch = pool->batches; batch; batch = batch->next) {
        for (size_t i = 0; i < batch->uninitialized; ++i) {
            Object *obj = &batch->objects[i];

            if (obj->reached) {
                assert(obj->type != OBJ_FREE);
                obj->reached = false;
            } else if (obj->type != OBJ_FREE) {
                assert(obj->transient_refcount == 0);
                FreeObject(obj, false);
                    // We don't need to free recursively, because with sweeping
                    // GC, we are guaranteed to eventually call FreeObject() on
                    // every dead object. In fact, freeing recursively would be
                    // problematic here, since it's not necessarily safe to run
                    // the reference counting GCs while a sweep is in progress.
            }
        }
    }

    pool->batches_since_last_gc = 0;
    ++pool->gc_collections;
}

BlimpGCStatistics ObjectPool_GetStats(ObjectPool *pool)
{
    BlimpGCStatistics stats = {
        .created = pool->seq,
        .min_clump = SIZE_MAX,
    };

    MarkReachable(pool);

    // Reset all the reached bits. As we go, count up the total number of
    // allocated objects, reachable objects, and the total
    // number of objects that have ever been initialized (which is the high
    // water mark for allocated objects).
    for (ObjectBatch *batch = pool->batches; batch; batch = batch->next) {
        stats.max_allocated += batch->uninitialized;
        for (size_t i = 0; i < batch->uninitialized; ++i) {
            Object *obj = &batch->objects[i];

            if (obj->type != OBJ_FREE) {
                ++stats.allocated;

                if (obj->entangled == NULL && obj->clump_next != obj) {
                    // This is the representative of a nontrivial clump.
                    ++stats.clumps;

                    // Count up the number of objects in the clump.
                    size_t clump_size = 0;
                    Object *curr = obj;
                    do {
                        ++clump_size;
                        curr = curr->clump_next;
                    } while (curr != obj);

                    stats.entangled += clump_size;
                    if (clump_size > stats.max_clump) {
                        stats.max_clump = clump_size;
                    }
                    if (clump_size < stats.min_clump) {
                        stats.min_clump = clump_size;
                    }
                }
            }
            if (obj->reached) {
                assert(obj->type != OBJ_FREE);
                ++stats.reachable;
            }
            obj->reached = false;
        }
    }

    stats.collections = pool->gc_collections;

    return stats;
}

////////////////////////////////////////////////////////////////////////////////
// Object creation and destruction
//

static Status NewObject(Blimp *blimp, Object *parent, Object **obj)
{
    assert(parent == NULL || parent->type != OBJ_FREE);

    // Try to allocate from the free list.
    if ((*obj = blimp->objects.free_list) != NULL) {
        assert((*obj)->type == OBJ_FREE);

        blimp->objects.free_list = (*obj)->next;
            // Pop the object off the free list.
        Scope_Clear(&(*obj)->scope);
            // The scope should already be initialized. Just make sure it's
            // empty.
    } else {
        // If that failed, try to allocate from the active batch.
        ObjectBatch *batch = blimp->objects.batches;

        if (
            // If it's time to do a garbage collection...
            blimp->options.gc_tracing &&
            blimp->objects.batches_since_last_gc >=
                blimp->options.gc_batches_per_trace &&

            // ...and the active batch is saturated...
            batch->uninitialized >= blimp->objects.batch_size
        ) {
            // Collect garbage and try the free list again.
            ObjectPool_CollectGarbage(&blimp->objects);
            *obj = blimp->objects.free_list;
        }

        if (*obj) {
            // If we got an object from the free list after garbage collecting,
            // prepare it to be allocated.
            blimp->objects.free_list = (*obj)->next;
            Scope_Clear(&(*obj)->scope);
        } else {
            // If garbage collection didn't turn up anything, try allocating
            // from the active batch.

            if (batch->uninitialized >= blimp->objects.batch_size) {
                // If the batch is saturated, allocate a new batch.
                TRY(Malloc(
                    blimp,
                    sizeof(ObjectBatch) +
                        sizeof(Object)*blimp->objects.batch_size,
                    &batch
                ));
                batch->uninitialized = 0;
                batch->next = blimp->objects.batches;
                blimp->objects.batches = batch;
                ++blimp->objects.batches_since_last_gc;
            }
            assert(batch->uninitialized < blimp->objects.batch_size);

            // Grab the next uninitialized object from the batch and initialize
            // it.
            *obj = &batch->objects[batch->uninitialized++];
            if (Scope_Init(blimp, &(*obj)->scope) != BLIMP_OK) {
                --batch->uninitialized;
                return Blimp_Reraise(blimp);
            }
            (*obj)->reached = false;

            if (blimp->options.gc_cycle_detection) {
                ERC_StaticInit(&blimp->objects, *obj);
            }
        }
    }

    (*obj)->parent = parent;
    (*obj)->seq = blimp->objects.seq++;
    (*obj)->internal_refcount = 0;
    (*obj)->transient_refcount = 0;
    ERC_Init(*obj);

    if (parent) {
        assert((*obj)->seq > parent->seq);
        BorrowParent(*obj);
    }

    BlimpObject_Borrow(*obj);
        // All objects are created with one transient reference.

    return BLIMP_OK;
}

static void FreeObject(Object *obj, bool recursive)
{
    if (obj->type == OBJ_FREE) {
        return;
    }

    assert(obj->transient_refcount == 0);
    obj->type = OBJ_FREE;

    Blimp *blimp = GetBlimp(obj);

    if (blimp->options.gc_cycle_detection) {
        ERC_RemoveFromClump(obj);
    }

    if (recursive) {
        // Release our references to all of the objects in this object's scope.
        for (ScopeIterator entry = Scope_Begin(&obj->scope);
             entry != Scope_End(&obj->scope);
             entry = Scope_Next(&obj->scope, entry))
        {
            Ref *ref = Scope_GetValue(&obj->scope, entry);
            ReleaseRef(obj, ref);
            FreeRef(blimp, ref);
        }

        // Release our reference to our parent.
        ReleaseParent(obj);
    }

    // Release our reference to the code expression if this is a block.
    if (obj->type == OBJ_BLOCK) {
        Blimp_FreeExpr(obj->code);
    }

    // Push the object onto the free list.
    obj->next = blimp->objects.free_list;
    blimp->objects.free_list = obj;
}

Status BlimpObject_NewBlock(
    Blimp *blimp,
    Object *parent,
    const Symbol *tag,
    Expr *code,
    Object **obj)
{
    HeapCheck(blimp);

    TRY(NewObject(blimp, parent, obj));
    (*obj)->type = OBJ_BLOCK;
    (*obj)->tag  = tag;
    (*obj)->code = code;
    ++code->refcount;

    HeapCheck(blimp);
    return BLIMP_OK;
}

Status BlimpObject_NewSymbol(
    Blimp *blimp, Object *parent, const Symbol *sym, Object **obj)
{
    TRY(NewObject(blimp, parent, obj));
    (*obj)->type = OBJ_SYMBOL;
    (*obj)->symbol = sym;

    HeapCheck(blimp);
    return BLIMP_OK;
}

Status BlimpObject_NewGlobal(Blimp *blimp, Object **obj)
{
    TRY(NewObject(blimp, NULL, obj));
    (*obj)->type = OBJ_GLOBAL;

    HeapCheck(blimp);
    return BLIMP_OK;
}

////////////////////////////////////////////////////////////////////////////////
// Object API
//

static Ref *Lookup(Object *obj, const Symbol *sym, Object **owner)
{
    Object *curr = obj;
    while (curr) {
        Ref *ret = Scope_Lookup(&curr->scope, sym);
        if (ret) {
            if (owner) {
                *owner = curr;
            }
            return ret;
        } else {
            curr = curr->parent;
        }
    }

    return NULL;
}

Object *BlimpObject_Parent(const Object *obj)
{
    assert(obj->type != OBJ_FREE);
    return obj->parent;
}

const Symbol *BlimpObject_Tag(const Object *obj)
{
    switch (obj->type) {
        case OBJ_BLOCK:
            return obj->tag;
        case OBJ_SYMBOL:
            return GetBlimp(obj)->objects.symbol_tag;
        case OBJ_GLOBAL:
            return NULL;
        default:
            assert(false);
            return NULL;
    }
}

void BlimpObject_Print(FILE *f, const Object *obj)
{
    switch (obj->type) {
        case OBJ_SYMBOL:
            fputs(obj->symbol->name, f);
            break;
        case OBJ_BLOCK:
            fprintf(f, "{%s|", obj->tag->name);
            Blimp_PrintExpr(f, obj->code);
            fprintf(f, "}");
            break;
        case OBJ_GLOBAL:
            fprintf(f, "<global>");
            break;
        default:
            assert(false);
    }
}

Status BlimpObject_ParseBlock(
    const Object *obj, const Symbol **tag, const Expr **code)
{
    if (obj->type != OBJ_BLOCK) {
        return Error(GetBlimp(obj), BLIMP_MUST_BE_BLOCK);
    }

    if (tag) {
        *tag = obj->tag;
    }
    if (code) {
        *code = obj->code;
    }
    return BLIMP_OK;
}

Status BlimpObject_ParseSymbol(const Object *obj, const Symbol **sym)
{
    if (obj->type != OBJ_SYMBOL) {
        return Error(GetBlimp(obj), BLIMP_MUST_BE_SYMBOL);
    }

    if (sym) {
        *sym = obj->symbol;
    }
    return BLIMP_OK;
}

Status BlimpObject_Get(const Object *obj, const Symbol *sym, Object **ret)
{
    assert(obj->type != OBJ_FREE);

    Ref *value = Lookup((Object *)obj, sym, NULL);
    if (value) {
        *ret = value->to;
        return BLIMP_OK;
    } else {
        return RuntimeErrorMsg(
            GetBlimp(obj),
            BLIMP_NO_SUCH_SYMBOL,
            "no symbol `%s' in scope", sym->name);
    }
}

Status BlimpObject_Set(Object *obj, const Symbol *sym, Object *val)
{
    Blimp *blimp = GetBlimp(obj);

    assert(obj->type != OBJ_FREE);
    assert(val->type != OBJ_FREE);

    // If the symbol is already in scope, update the existing value.
    Object *owner;
    Ref *ref = Lookup(obj, sym, &owner);
    if (ref) {
        ReleaseRef(owner, ref);
            // This scope owned a reference to the existing value. After this
            // operation, the old value will no longer be reachable through this
            // scope, so we have to release our reference.

        ref->to = val;
            // Reinitialize the reference to point to the new value.

        BorrowRef(owner, ref);
            // Borrow the new value on behalf of the existing owner of the scope
            // entry.
    } else {
        // Otherwise, add it to the innermost scope.
        TRY(AllocRef(blimp, &ref));
        ref->to = val;
        TRY(Scope_Update(&obj->scope, sym, ref));
        BorrowRef(obj, ref);
    }

    HeapCheck(blimp);
    return BLIMP_OK;
}

Status BlimpObject_Eval(Object *obj, Object **ret)
{
    if (obj->type != OBJ_BLOCK) {
        return ErrorMsg(
            GetBlimp(obj),
            BLIMP_MUST_BE_BLOCK,
            "can only evaluate a block object");
    }

    return Blimp_Eval(GetBlimp(obj), obj->code, obj, ret);
        // Evaluate the code expression in `obj`s scope.
}


Object *BlimpObject_Borrow(Object *obj)
{
    Blimp *blimp = GetBlimp(obj);

    ++obj->transient_refcount;

    if (blimp->options.gc_cycle_detection) {
        ERC_ExternalBorrow(obj);
    }

    return obj;
}

void BlimpObject_Release(Object *obj)
{
    Blimp *blimp = GetBlimp(obj);

    assert(obj->type != OBJ_FREE);
    assert(obj->transient_refcount);
    --obj->transient_refcount;

    if (blimp->options.gc_cycle_detection) {
        ERC_ExternalRelease(obj);
    }

    if (obj->transient_refcount == 0 &&
        obj->internal_refcount == 0 &&
        blimp->options.gc_refcount)
    {
        FreeObject(obj, true);
    }

    HeapCheck(blimp);
}
