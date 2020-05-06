#include "internal/blimp.h"
#include "internal/hash_map.h"
#include "internal/symbol.h"

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
// Object API
//

static inline Blimp *GetBlimp(const Object *obj)
{
    return HashMap_GetBlimp(&obj->scope);
}

static Object **Lookup(const Object *obj, const Symbol *sym)
{
    const Object *curr = obj;
    while (curr) {
        Object **ret = Scope_Lookup(&obj->scope, sym);
        if (ret) {
            return ret;
        } else {
            curr = curr->parent;
        }
    }

    return NULL;
}

Object *BlimpObject_Parent(Object *obj)
{
    return obj->parent;
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
    *ret = *Lookup(obj, sym);
    if (*ret) {
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
    // If the symbol is already in scope, update the existing value.
    Object **existing_value = Lookup(obj, sym);
    if (existing_value) {
        BlimpObject_Release(*existing_value);
            // This scope owned a reference to the existing value. After this
            // operation, the old value will no longer be reachable through this
            // scope, so we have to release our reference.
        *existing_value = BlimpObject_Borrow(val);
            // Acquire a reference to the new value, since we're about to store
            // it persistently. This reference will be released when the new
            // value is replaced with a newer value, or when the object itself
            // is destroyed.
        return BLIMP_OK;
    }

    // Otherwise, add it to the innermost scope.
    TRY(Scope_Update(&obj->scope, sym, val));
    BlimpObject_Borrow(val);
        // We have stored a persistent reference to `val`.

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
// To fix this, we will eventually need to implement some form of tracing GC. We
// can add that on top of the reference counting framework, though, using
// reference counting to reclaim memory with low latency where possible, and
// only pausing for an expensive GC sweep when reference counting is unable to
// reclaim sufficient memory for the program to continue.
//
// Note that the only way to create circular references between objects is to
// create circular references through scopes using BlimpObject_Set, because
// scopes are the only way for objects to reference each other. Therefore, we
// don't have to worry about cycles created by users of the object API. As long
// as we have a tracing GC solution that can detect cycles within the scope
// graph, then we should be able to perfectly reclaim memory as long as users of
// the API call BlimpObject_Release when they should. This means that we can add
// tracing GC transparently, without changing the BlimpObject_Borrow/Release
// API.
//

typedef struct ObjectBatch {
    size_t uninitialized;
    struct ObjectBatch *next;
    Object objects[];
} ObjectBatch;

Status ObjectPool_Init(Blimp *blimp, ObjectPool *pool)
{
    // Compute batch size in terms of number of objects, rather than bytes, and
    // round up to ensure we get at least one object per batch.
    pool->batch_size =
        (blimp->options.object_pool_batch_size/sizeof(Object)) + 1;

    TRY(Malloc(
        blimp,
        sizeof(ObjectBatch) + sizeof(Object)*pool->batch_size,
        &pool->batches
    ));
    pool->batches->uninitialized = 0;
    pool->batches->next = NULL;
    pool->free_list = NULL;
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

static Status New(Blimp *blimp, Object *parent, Object **obj)
{
    // Try to allocate from the free list.
    if ((*obj = blimp->objects.free_list) != NULL) {
        assert((*obj)->type == OBJ_FREE);

        blimp->objects.free_list = (*obj)->next;
            // Pop the object off the free list.
        Scope_Clear(&(*obj)->scope);
            // The scope should already be initialized. Just make sure it's
            // empty.

        assert((*obj)->refcount == 0);
        (*obj)->refcount = 1;
        (*obj)->parent = parent;
        return BLIMP_OK;
    }

    // If that failed, try to allocate from the active batch.
    ObjectBatch *batch = blimp->objects.batches;
    if (batch->uninitialized >= blimp->objects.batch_size) {
        // If the batch is saturated, allocate a new batch.
        TRY(Malloc(
            blimp,
            sizeof(ObjectBatch) + sizeof(Object)*blimp->objects.batch_size,
            &batch
        ));
        batch->uninitialized = 0;
        batch->next = blimp->objects.batches;
        blimp->objects.batches = batch;
    }
    assert(batch->uninitialized < blimp->objects.batch_size);

    // Grab the next uninitialized object from the batch and initialize it.
    *obj = &batch->objects[batch->uninitialized++];
    if (Scope_Init(blimp, &(*obj)->scope) != BLIMP_OK) {
        --batch->uninitialized;
        return Blimp_Reraise(blimp);
    }

    (*obj)->parent = parent;
    (*obj)->refcount = 1;
    return BLIMP_OK;
}

Status BlimpObject_NewBlock(
    Blimp *blimp,
    Object *parent,
    const Symbol *tag,
    const Expr *code,
    Object **obj)
{
    TRY(New(blimp, parent, obj));
    (*obj)->type = OBJ_BLOCK;
    (*obj)->tag  = tag;
    (*obj)->code = code;
    return BLIMP_OK;
}

Status BlimpObject_NewSymbol(
    Blimp *blimp, Object *parent, const Symbol *sym, Object **obj)
{
    TRY(New(blimp, parent, obj));
    (*obj)->type = OBJ_SYMBOL;
    (*obj)->symbol = sym;
    return BLIMP_OK;
}

Object *BlimpObject_Borrow(Object *obj)
{
    assert(obj->refcount);
    ++obj->refcount;
    return obj;
}

void BlimpObject_Release(Object *obj)
{
    Blimp *blimp = GetBlimp(obj);

    assert(obj->refcount > 0);
    if (--obj->refcount == 0) {
        // Release our references to all of the objects in this object's scope.
        for (ScopeIterator entry = Scope_Begin(&obj->scope);
             entry != Scope_End(&obj->scope);
             entry = Scope_Next(&obj->scope, entry))
        {
            BlimpObject_Release(Scope_GetValue(&obj->scope, entry));
        }

        // Push the object onto the free list.
        obj->type = OBJ_FREE;
        obj->next = blimp->objects.free_list;
        blimp->objects.free_list = obj;
    }
}
