#include "internal/checkpoint_alloc.h"
#include "internal/error.h"

typedef struct CheckpointBlock Block;

Status CheckpointAllocator_Init(
    Blimp *blimp, CheckpointAllocator *alloc, size_t block_size)
{
    // Allocate one block, to guarantee that `blocks` is never empty.
    TRY(Malloc(blimp, sizeof(Block) + block_size, &alloc->blocks));
    alloc->blocks->next = NULL;
    alloc->blocks->allocated = 0;

    alloc->active_block = alloc->blocks;
        // The only block is necessarily the active block...
    alloc->last_block = alloc->blocks;
        // ...and the last one.

    alloc->block_size = block_size;
    return BLIMP_OK;
}

void CheckpointAllocator_Destroy(CheckpointAllocator *alloc)
{
    while (alloc->blocks != NULL) {
        Block *block = alloc->blocks;
        alloc->blocks = block->next;
        free(block);
    }
}

void *CheckpointAllocator_Alloc(
    CheckpointAllocator *alloc, size_t size)
{
    assert(size <= alloc->block_size);

    if (alloc->active_block->allocated + size > alloc->block_size) {
        // If we don't have room in the active block to handle this allocation,
        // advance to the next block.
        if (alloc->active_block->next == NULL) {
            // If there is no next block, get one from system malloc().
            alloc->active_block->next = malloc(
                sizeof(Block) + alloc->block_size);
            if (alloc->active_block->next == NULL) {
                return NULL;
            }
            alloc->active_block->next->next = NULL;
        }

        // Whenever we advance to a new block, it is empty.
        alloc->active_block->next->allocated = 0;
        alloc->active_block = alloc->active_block->next;
    }

    assert(alloc->active_block != NULL);
    assert(alloc->active_block->allocated + size <= alloc->block_size);

    // Offset into the block by the number of currently allocated bytes, and
    // then increment the byte count to include the new allocation.
    void *result =
        (char *)alloc->active_block->memory + alloc->active_block->allocated;
    alloc->active_block->allocated += size;

    return result;
}

void CheckpointAllocator_Clear(CheckpointAllocator *alloc)
{
    alloc->active_block = alloc->blocks;
    alloc->active_block->allocated = 0;
}

void CheckpointAllocator_Checkpoint(
    CheckpointAllocator *alloc, AllocatorCheckpoint *checkpoint)
{
    checkpoint->block = alloc->active_block;
    checkpoint->allocated = alloc->active_block->allocated;
}

void CheckpointAllocator_RestoreCheckpoint(
    CheckpointAllocator *alloc, const AllocatorCheckpoint *checkpoint)
{
    alloc->active_block = checkpoint->block;
    alloc->active_block->allocated = checkpoint->allocated;
}
