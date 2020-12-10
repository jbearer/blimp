////////////////////////////////////////////////////////////////////////////////
// Object pool
//
////////////////////////////////////////////////////////////////////////////////
// Motivation
//
// A typical bl:mp program can put quite a heavy load on the memory allocator
// by making lots of short-lived allocations. These allocations tend to all be
// for the same kind of object (or one of a few kinds of objects), so a custom
// allocator can take advantage of the fact that
//  1. All allocations are the same size, and
//  2. All allocations have the same fields, and thus certain fields can be
//     initialized once and then preserved across reuses of the same memory,
//     saving costs of reinitializing objects.
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
// allocating memory for dynamically sized data structures owned by the object.
// This scheme does require some extra
// accounting, though: we need to know which free objects are partially
// initialized, so we don't waste time reinitializing them, and which are
// uninitialized so we can initialize them before allocating them. The
// initialized free objects are linked together in a free object list which may
// traverse objects in all the batches in the system, in an arbitrary order. The
// uninitialized objects are handled per batch: each batch allocates
// uninitialized objects in order, starting from the first object in its array
// of objects, and it keeps track of the number of objects it has initialized so
// far.
//
// Finally, all the batches themselves are linked together in another list, in
// the order in which they were allocated from the system allocator, with the
// most recently allocated batch first. Since we only allocate a new batch when
// there are no free objects to allocate out of the existing batches, all of the
// batches in this list except possibly the first contain only initialized
// objects. We refer to these as the saturated batches. The first batch in the
// list may contain uninitialized objects which can be allocated; if it does, we
// call it the active batch.
//
// We never actually use the list of batches for allocation; if we want to
// allocate an uninitialized object we always do it out of the active batch, and
// if we want an initialized object that may come from a saturated batch, we
// access it via the free list, not the list of batches. However, the list of
// batches is useful for iterating over live objects, and it is used at system
// teardown to return each of the allocated batches to the system allocator.
//
//  batch list                                          free list
//   |                                                   |
//  _V____________________________       ________________|________________
// | Next: -----------------------|---->| Next: ---------|----------------|->...
// | Initialized: 2               |     | Initialized: batch_size         |
// | Objects:___________________  |     | Objects:_______V______________  |
// |   | FREE | OBJ | ??? | ??? | |     |   |  OBJ  | FREE | OBJ | FREE | |
// |   |_|__^_|_____|_____|_____| |     |   |_______|__|___|_____|__^___| |
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
//     partially initialized object off the head of a linked list.
//  2. The active batch: slightly more expensive, requires initializing the
//     object, but does not require a malloc.
//  3. Allocating a new batch: expensive, requires calling malloc (though note
//     that we do not have to initalize the whole batch, since we do that lazily
//     when we allocate from the batch in tier 2). Done very rarely because of
//     batching.
//
////////////////////////////////////////////////////////////////////////////////
// Deallocating
//
// Deallocating is very simple: we put the object to be deallocated as is on the
// front of the free list. Note that we never return memory to the system
// allocator until the entire ObjectPool is torn down -- even if a batch becomes
// completely free at some point during its life.
//
// When an object is deallocated, it becomes a "free object". In a normal,
// general-purpose allocator, free objects are owned exclusively by the
// allocator, and the allocator is allowed to overwrite any and all of their
// contents with its own metadata. However, the entire point of this allocator
// is to allow objects to remain partially initialized between being freed and
// being reallocated, so this allocator makes a strong guarantee about
// preserving the contents of objects even after they are freed:
//
// The second pointer-sized word of each free object is reserved by the
// allocator, and may be overwritten after an object is freed. All other
// contents of a freed object will be left unchanged by the allocator.
//

#ifndef BLIMP_POOL_ALLOC_H
#define BLIMP_POOL_ALLOC_H

#include "common.h"

struct Batch {
    size_t initialized;
    struct Batch *next;
    char memory[];
};

typedef void(*PoolGCFunc)(void *arg);
typedef Status(*PoolInitFunc)(void *object, void *arg);

typedef struct {
    size_t batch_size;
    size_t batches_per_gc;
    size_t batches_since_last_gc;
    size_t object_size;
    struct Batch *batches;
    struct FreeNode *free_list;
    PoolInitFunc initialize;
    PoolGCFunc gc;
    void *arg;
} PoolAllocator;

typedef struct {
    struct Batch *batch;
    size_t object;
} PoolAllocatorIterator;

/**
 * \brief Initialize an allocator for objects of a given size.
 *
 * \param object_size
 *      The size in bytes of a single object.
 * \param batch_size
 *      The size in bytes of a batch of objects.
 * \param batches_per_gc
 *      If this pool is garbage collected, this is the number of consecutive
 *      batches to take from the system allocator before trying to make space in
 *      existing batches using GC. If this pool is not garbage collected, this
 *      parameter is ignored.
 * \param initialize
 *      If non-NULL, this callback will be called whenever a new object is first
 *      initialized from a batch.
 * \param gc
 *      If non-NULL, this callback will be called whenever there are no free or
 *      uninitialized objects in the existing batches and `batches_per_gc` new
 *      batches have been allocated from the system since the last garbage
 *      collection. There are no constraints on the implementation of this
 *      function (it is always allowed to fail) but it should attempt to free
 *      objects which are currently allocated.
 * \param arg
 *      Arbitrary data to be passed as an argument to `initialize` and `gc`. If
 *      both `initialize` and `gc` are NULL, this parameter is ignored.
 */
PRIVATE void PoolAllocator_Init(
    PoolAllocator *pool,
    size_t object_size,
    size_t batch_size,
    size_t batches_per_gc,
    PoolInitFunc initialize,
    PoolGCFunc gc,
    void *arg);

/**
 * \brief Return allocated batches to the system allocator.
 */
PRIVATE void PoolAllocator_Destroy(PoolAllocator *pool);

/**
 * \brief Allocate an object from the pool.
 *
 * \return
 *      A pointer to a contiguous memory region of at least `object_size` bytes,
 *      or NULL on failure.
 */
PRIVATE void *PoolAllocator_Alloc(PoolAllocator *pool);

/**
 * \brief Return an object allocated by PoolAllocator_Alloc() to the pool.
 */
PRIVATE void PoolAllocator_Free(PoolAllocator *pool, void *p);

/**
 * \brief
 *      Return the maximum number of objects which have been allocated from this
 *      pool at one time.
 *
 * \note
 *      This function traverses the entire heap, and can be quite slow. Use it
 *      with caution.
 */
PRIVATE size_t PoolAllocator_HighWaterMark(PoolAllocator *pool);

/**
 * \brief Get an iterator which will traverse all initialized objects.
 */
static inline PoolAllocatorIterator PoolAllocator_Begin(PoolAllocator *pool)
{
    return (PoolAllocatorIterator) {pool->batches, 0};
}

/**
 * \brief Advance an iterator to the next object, and return the current one.
 *
 * \return
 *      A pointer to a contiguous memory region of at least `object_size` bytes,
 *      or NULL if the iterator has reached the end of the pool.
 */
static inline void *PoolAllocator_Next(
    PoolAllocator *pool, PoolAllocatorIterator *it)
{
    if (it->batch == NULL) {
        return NULL;
    }

    if (it->object >= it->batch->initialized) {
        it->batch = it->batch->next;
        it->object = 0;
        if (it->batch == NULL) {
            return NULL;
        }

        assert(it->object < it->batch->initialized);
            // If there is more than one batch, then all batches after the first
            // one (so, the current batch in particular) are saturated, meaning
            // they have `batch_size > 0` initialized objects.
    }

    return it->batch->memory + pool->object_size*it->object++;
}

#endif
