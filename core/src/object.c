#include "internal/blimp.h"
#include "internal/expr.h"
#include "internal/hash_map.h"
#include "internal/symbol.h"

bool Reachable(ObjectPool *pool, Object *obj);

////////////////////////////////////////////////////////////////////////////////
// Scopes: just a wrapper around a hash map mapping symbols to objects
//

static inline Status Scope_Init(Blimp *blimp, Scope *scope)
{
    HashMapOptions options = HASH_MAP_DEFAULT_OPTIONS;
    options.create_empty = true;
        // Create the scope with no associated memory. This is an optimization:
        // since many bl:mp objects (especially symbols) never have values
        // stored in their scopes, it doesn't make sense to proactively allocate
        // memory until we are ready to store something in the scope.

    return HashMap_Init(
        blimp, scope,
        sizeof(Symbol *), sizeof(Object *),
        (EqFunc)SymbolEq, (HashFunc)SymbolHash,
        &options);
}

static inline void Scope_Clear(Scope *scope)
{
    HashMap_Clear(scope);
}

// Get a reference to the value of `sym` in the given scope, or `NULL`.
static inline Object **Scope_Lookup(const Scope *scope, const Symbol *sym)
{
    return HashMap_Find(scope, &sym);
}

static inline Status Scope_Update(Scope *scope, const Symbol *sym, Object *obj)
{
    return HashMap_Update(scope, &sym, &obj);
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

static inline Object *Scope_GetValue(Scope *scope, ScopeIterator it)
{
    return *(Object **)HashMap_GetValue(scope, it);
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
// The current implementation uses reference counting as a simplistic form of
// garbage collection. Every allocated object has a nonzero reference count. The
// reference count is incremented every time the object is stored in a scope.
// This is the only internal place where we create persistent references to
// objects. The reference count is decremented whenever an object in a scope is
// replaced by a different object. There is also an API for manipulating
// reference counts (BlimpObject_Borrow and BlimpObject_Release) so users of
// libblimpcore can manage their own references.
//
// Whenever the reference count on an object becomes 0, we return that object to
// the object pool using the deallocation algorithm described above. We also
// decrement the reference counts of all of the objects stored in the deleted
// object's scope, since they are no longer reachable through that scope. We may
// end up destroying some of those objects recursively, if their reference
// counts are now 0.
//
// The reference count on an object is actually subdivided into two counts:
//  * `transient_refcount` counts external, unmanaged references obtained by
//    BlimpObject_Borrow().
//  * `internal_refcount` counts managed references between objects, including
//    scope references and parent pointers.
//
// Using reference counting for garbage collection has some advantages:
//  * It is easy to implement and understand.
//  * It is low-overhead, allowing high throughput.
//  * It is less prone to long, unpredictable pauses than tracing GC, allowing
//    low latency.
//  * It is easy to expose an API with just two functions.
//
// On top of these advantages, reference counting should be sufficient for many
// kinds of programs to run without exhausting the heap. However, in the general
// case, reference counting is not a complete form of garbage collection,
// because it cannot detect cycles in the heap. Programs that create circular
// references and then leak them may run out of memory after some time.
//
// To fix this, we have a tracing garbage collector to supplement reference
// counting. The tracing GC is a very simple mark-sweep algorithm which frees
// all objects not reachable from a root. A "root" is defined as any object with
// a nonzero transient reference count; those are the objects that the program
// on top of us is currently using, including the global object.
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
    return BLIMP_OK;
}

void ObjectPool_Destroy(Blimp *blimp, ObjectPool *pool)
{
    ObjectBatch *batch = pool->batches;
    while (batch) {
        ObjectBatch *next = batch->next;
        Free(blimp, &batch);
        batch = next;
    }
}

static void FreeObject(Object *obj);

static inline Object *InternalBorrow(Object *owner, Object *obj)
{
    // Only increment the referece count if the object is different from the
    // owner. There's no need to count references-to-self, because it is
    // impossible for an object to be destroyed before itself is destroyed.
    if (obj != owner) {
        ++obj->internal_refcount;
    }
    return obj;
}

static inline void InternalRelease(Object *owner, Object *obj)
{
    if (obj == owner) {
        // If `obj == owner`, we didn't acquire a reference in InternalBorrow(),
        // so there's nothing to release.
        return;
    }

    if (obj->internal_refcount == 0) {
        // It's possible that we have a reference to an object with an internal
        // refcount of 0 if that object has already been freed by the garbage
        // collector. In that case, the garbage collector has determined that
        // `obj` is unreachable, and we are unreachable as well. `obj` has
        // already been freed, and we are about to be freed, so there's nothing
        // more to do.
        return;
    }

    if (--obj->internal_refcount == 0 && obj->transient_refcount == 0) {
        FreeObject(obj);
    }
}

static inline Blimp *GetBlimp(const Object *obj)
{
    return HashMap_GetBlimp(&obj->scope);
}

static Status NewObject(Blimp *blimp, Object *parent, Object **obj)
{
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
        }
    }

    if (parent) {
        InternalBorrow(*obj, parent);
    }

    (*obj)->parent = parent;
    (*obj)->internal_refcount = 0;
    (*obj)->transient_refcount = 1;
    return BLIMP_OK;
}

static void FreeObject(Object *obj)
{
    Blimp *blimp = GetBlimp(obj);

    assert(obj->transient_refcount == 0);
    obj->internal_refcount = 0;

    // Release our references to all of the objects in this object's scope.
    for (ScopeIterator entry = Scope_Begin(&obj->scope);
         entry != Scope_End(&obj->scope);
         entry = Scope_Next(&obj->scope, entry))
    {
        Object *child = Scope_GetValue(&obj->scope, entry);
        InternalRelease(obj, child);
    }

    // Release our reference to our parent.
    if (obj->parent) {
        InternalRelease(obj, obj->parent);
    }

    // Release our reference to the code expression if this is a block.
    if (obj->type == OBJ_BLOCK) {
        Blimp_FreeExpr(obj->code);
    }

    // Push the object onto the free list.
    obj->type = OBJ_FREE;
    obj->next = blimp->objects.free_list;
    blimp->objects.free_list = obj;
}

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
            Object *child = Scope_GetValue(&obj->scope, it);
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
                FreeObject(obj);
            }
        }
    }

    pool->batches_since_last_gc = 0;
}

BlimpGCStatistics ObjectPool_GetStats(ObjectPool *pool)
{
    BlimpGCStatistics stats = {0};

    MarkReachable(pool);

    // Reset all the reached bits. As we go, count up the total number of
    // allocated objects, reachable objects, and the total
    // number of objects that have ever been initialized (which is the high
    // water mark for allocated objects).
    for (ObjectBatch *batch = pool->batches; batch; batch = batch->next) {
        stats.max_allocated += batch->uninitialized;
        for (size_t i = 0; i < batch->uninitialized; ++i) {
            if (batch->objects[i].type != OBJ_FREE) {
                ++stats.allocated;
            }
            if (batch->objects[i].reached) {
                ++stats.reachable;
            }
            batch->objects[i].reached = false;
        }
    }

    return stats;
}

Status BlimpObject_NewBlock(
    Blimp *blimp,
    Object *parent,
    const Symbol *tag,
    Expr *code,
    Object **obj)
{
    TRY(NewObject(blimp, parent, obj));
    (*obj)->type = OBJ_BLOCK;
    (*obj)->tag  = tag;
    (*obj)->code = code;
    ++code->refcount;
    return BLIMP_OK;
}

Status BlimpObject_NewSymbol(
    Blimp *blimp, Object *parent, const Symbol *sym, Object **obj)
{
    TRY(NewObject(blimp, parent, obj));
    (*obj)->type = OBJ_SYMBOL;
    (*obj)->symbol = sym;
    return BLIMP_OK;
}

Status BlimpObject_NewGlobal(Blimp *blimp, Object **obj)
{
    TRY(NewObject(blimp, NULL, obj));
    (*obj)->type = OBJ_GLOBAL;
    return BLIMP_OK;
}

Object *BlimpObject_Borrow(Object *obj)
{
    ++obj->transient_refcount;
    return obj;
}

void BlimpObject_Release(Object *obj)
{
    assert(obj->transient_refcount);

    if (--obj->transient_refcount == 0 && obj->internal_refcount == 0) {
        FreeObject(obj);
    }
}

////////////////////////////////////////////////////////////////////////////////
// Object API
//

static Object **Lookup(Object *obj, const Symbol *sym, Object **owner)
{
    Object *curr = obj;
    while (curr) {
        Object **ret = Scope_Lookup(&curr->scope, sym);
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
    Object **value = Lookup((Object *)obj, sym, NULL);
    if (value) {
        *ret = *value;
        return BLIMP_OK;
    } else {
        return ErrorMsg(
            GetBlimp(obj),
            BLIMP_NO_SUCH_SYMBOL,
            "no symbol `%s' in scope", sym->name);
    }
}

Status BlimpObject_Set(Object *obj, const Symbol *sym, Object *val)
{
    assert(val);

    // If the symbol is already in scope, update the existing value.
    Object *owner;
    Object **existing_value = Lookup(obj, sym, &owner);
    if (existing_value) {
        InternalBorrow(owner, val);
            // Borrow the new value on behalf of the existing owner of the scope
            // entry.

        Object *to_free = *existing_value;
        *existing_value = val;
        if (to_free != owner) {
            InternalRelease(owner, to_free);
                // This scope owned a reference to the existing value. After
                // this operation, the old value will no longer be reachable
                // through this scope, so we have to release our reference.
        }

        return BLIMP_OK;
    }

    // Otherwise, add it to the innermost scope.
    InternalBorrow(obj, val);
    TRY(Scope_Update(&obj->scope, sym, val));
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
