////////////////////////////////////////////////////////////////////////////////
// Checkpoint Allocator
//
// The checkpoint allocator is designed for phase-based algorithms which
// allocate a large number of objects very quickly (during a phase) and then all
// of those objects become free at around the same time (the end of the phase).
// It supports rapid allocation of variably sized objects, and it simplifies
// object lifetime management by providing an interface to deallocate all
// objects allocated during a phase at once. This bulk deallocation process is
// also much faster than deallocating objects individually.
//
// In addition, the checkpoint allocator provides its namesake checkpointing
// functionality, which allows for a limited form of freeing a subset of objects
// during a phase: the application can set a checkpoint, allocate some objects,
// and then return to the checkpoint, freeing all objects which were allocated
// since the checkpoint was created but retaining objects created earlier in the
// phase.
//
// The allocator does not support freeing of individual objects. The freedom
// this provides for the implementation is what allows allocation to be so fast.
//
////////////////////////////////////////////////////////////////////////////////
// Implementation
//
// A checkpoint allocator consists of a linked list of large, contiguous blocks
// of memory. There is a pointer to an active block, which is one of the blocks
// in the list. Each block store the number of bytes in that block which are
// allocated. Allocation proceeds sequentially from the first block on down the
// list, and within each block allocation proceeds sequentially by memory
// address. If an allocation cannot fit in the remaining unallocated portion of
// the active block, the next block in the list becomes the new active block and
// the allocation is retried. If there is no next block in the list, a new one
// is allocated from the system allocator (malloc()).
//
// To free all allocated objects, all we have to do is reseat the active block
// pointer to point to the first block in the list, and set that block's count
// of allocated bytes to 0. Note that we do not have to clear the allocated byte
// count of other blocks since, due to the sequential nature of allocation,
// whenever we advance the active block pointer, we konw the new block is
// completely unallocated, and we can clear its count at that time.
//
// To create a checkpoint, we simply store the active block pointer and the
// allocated byte count within that block at the time the checkpoint is created.
// These can easily be used to restore the whole allocator state when we return
// to the checkpoint.
//

#ifndef BLIMP_CHECKPOINT_ALLOC_H
#define BLIMP_CHECKPOINT_ALLOC_H

#include "common.h"

struct CheckpointBlock {
    size_t allocated;
    struct CheckpointBlock *next;
    char memory[];
};

typedef struct {
    struct CheckpointBlock *blocks;
        // Head of the list of blocks. This list is always non-empty once the
        // allocator has been initialized, so this is guaranteed to be non-NULL.
    struct CheckpointBlock *active_block;
    struct CheckpointBlock *last_block;
        // The last block in the list of `blocks`, so that we can append to the
        // back of the list.
    size_t block_size;
} CheckpointAllocator;

typedef struct {
    struct CheckpointBlock *block;
    size_t allocated;
} AllocatorCheckpoint;

/**
 * \brief Initialize a checkpoint allocator.
 *
 * \param block_size
 *      The number of allocatable bytes in each block. The actual size of a
 *      block requested from the system allocator will be slightly larger to
 *      hold allocator metadata.
 */
PRIVATE Status CheckpointAllocator_Init(
    Blimp *blimp, CheckpointAllocator *alloc, size_t block_size);

/**
 * \brief Return all blocks to the system allocator.
 */
PRIVATE void CheckpointAllocator_Destroy(CheckpointAllocator *alloc);

/**
 * \brief Allocate a contiguous block of at least `size` bytes.
 *
 * \pre `size <= block_size`
 *
 * \return
 *      A pointer to the allocated block, or `NULL` on failure. Allocation can
 *      fail if there is not enough space in any of the allocated blocks and if
 *      the system allocator does not have enough contiguous free memory to
 *      allocate a new block.
 */
PRIVATE void *CheckpointAllocator_Alloc(
    CheckpointAllocator *alloc, size_t size);

/**
 * \brief Free all memory allocated from this allocator.
 */
PRIVATE void CheckpointAllocator_Clear(CheckpointAllocator *alloc);

/**
 * \brief Save allocator state to later partially free allocated memory.
 */
PRIVATE void CheckpointAllocator_Checkpoint(
    CheckpointAllocator *alloc, AllocatorCheckpoint *checkpoint);

/**
 * \brief
 *      Free all memory that has been allocated since `checkpoint` was created.
 *
 * In addition to freeing memory allocated since `checkpoint` was created, this
 * function also invalidates any checkpoints which were created after
 * `checkpoint`. Checkpoints created before it can still be used with
 * CheckpointAllocator_RestoreCheckpoint(), but checkpoints created after it may
 * not. Upholding this constraint is the caller's responsibility, and it is not
 * checked at runtime.
 */
PRIVATE void CheckpointAllocator_RestoreCheckpoint(
    CheckpointAllocator *alloc, const AllocatorCheckpoint *checkpoint);

#endif
