#ifndef BLIMP_OBJECT_H
#define BLIMP_OBJECT_H

#include "internal/common.h"
#include "internal/hash_map.h"

typedef HashMap Scope;

struct BlimpObject {
    // General object data.
    BlimpObject *parent;
    Scope scope;
    size_t refcount;

    // Type-specific data.
    enum {
        OBJ_SYMBOL,
        OBJ_BLOCK,
        OBJ_FREE,
    } type;
    union {
        const Symbol *symbol;   // Symbol for symbol objects
        struct {
            const Symbol *tag;  // Class of blocks
            const Expr   *code; // Body of blocks
        };
        Object       *next;     // Next free object in a free list.
    };
};

// An ObjectPool is an allocator for Objects, optimized for fast creation,
// destruction, and reuse of short-lived objects, which are very common in bl:mp
// programs. See the comments in object.c for details on the implementation.
typedef struct {
    struct ObjectBatch *batches;
    Object *free_list;
} ObjectPool;

PRIVATE Status ObjectPool_Init(Blimp *blimp, ObjectPool *pool);
PRIVATE void ObjectPool_Destroy(Blimp *blimp, ObjectPool *pool);

#endif
