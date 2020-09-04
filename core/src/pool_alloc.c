#include "internal/pool_alloc.h"

typedef struct Batch Batch;

typedef struct FreeNode {
    size_t header;
        // The first word of the FreeNode struct is not used by the pool
        // allocator. It is reserved for use as a header by the user of the
        // allocator, even when objects are on the free list, so that it is
        // possible to check if an object is free or not using information
        // stored in the header.
    struct FreeNode *next;
} FreeNode;

void PoolAllocator_Init(
    PoolAllocator *pool,
    size_t object_size,
    size_t batch_size,
    size_t batches_per_gc,
    Status(*initialize)(void *object, void *arg),
    void(*gc)(void *arg),
    void *arg)
{
    assert(gc == NULL || batches_per_gc > 0);

    if (object_size < sizeof(FreeNode)) {
        // We need to be able to fit a FreeNode in place of each object.
        object_size = sizeof(FreeNode);
    }

    pool->object_size = object_size;
    pool->batch_size = batch_size/object_size + 1;
        // Compute batch size in terms of number of objects, rather than bytes,
        // and round up to ensure we get at least one object per batch.
    pool->batches_per_gc = batches_per_gc;
    pool->batches_since_last_gc = 0;
    pool->batches = NULL;
    pool->free_list = NULL;
    pool->initialize = initialize;
    pool->gc = gc;
    pool->arg = arg;
}

void PoolAllocator_Destroy(PoolAllocator *pool)
{
    Batch *batch = pool->batches;
    while (batch != NULL) {
        Batch *next = batch->next;
        free(batch);
        batch = next;
    }
}

void *PoolAllocator_Alloc(PoolAllocator *pool)
{
    FreeNode *obj = NULL;

    // Try to allocate from the free list.
    if ((obj = pool->free_list) != NULL) {
        pool->free_list = obj->next;
            // Pop the object off the free list.
    } else {
        // If that failed, try to allocate from the active batch.
        Batch *batch = pool->batches;

        if (
            // If it's time to do a garbage collection...
            pool->gc &&
            pool->batches_since_last_gc >= pool->batches_per_gc &&

            // ...and the active batch is saturated...
            batch->initialized >= pool->batch_size
        ) {
            // Collect garbage and try the free list again.
            pool->gc(pool->arg);
            pool->batches_since_last_gc = 0;
            obj = pool->free_list;
        }

        if (obj != NULL) {
            // If we got an object from the free list after garbage collecting,
            // remove it from the list.
            pool->free_list = obj->next;
        } else {
            // If garbage collection didn't turn up anything, try allocating
            // from the active batch.

            if (batch == NULL || batch->initialized >= pool->batch_size) {
                // If the batch is saturated, allocate a new batch.
                batch = malloc(
                    sizeof(Batch) + pool->batch_size*pool->object_size);
                if (batch == NULL) {
                    // We've tried everything, there simply is no more memory.
                    return NULL;
                }

                batch->initialized = 0;
                batch->next = pool->batches;
                pool->batches = batch;
                ++pool->batches_since_last_gc;
            }
            assert(batch->initialized < pool->batch_size);

            // Grab the next uninitialized object from the batch and initialize
            // it.
            obj = (FreeNode *)
                (batch->memory + pool->object_size*batch->initialized++);
            if (pool->initialize != NULL &&
                pool->initialize(obj, pool->arg) != BLIMP_OK)
            {
                --batch->initialized;
                return NULL;
            }
        }
    }

    return obj;
}

void PoolAllocator_Free(PoolAllocator *pool, void *p)
{
    FreeNode *obj = (FreeNode *)p;
    obj->next = pool->free_list;
    pool->free_list = obj;
}

size_t PoolAllocator_HighWaterMark(PoolAllocator *pool)
{
    size_t n = 0;

    // We never initialize a new object when there is a free object available,
    // so the maximum number of simultaneously allocated objects is just the
    // number of objects we've initialized.
    for (Batch *batch = pool->batches; batch; batch = batch->next) {
        n += batch->initialized;
    }

    return n;
}
