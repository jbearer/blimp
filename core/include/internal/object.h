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
    size_t children;

    // Reachability analysis.
    bool reached;
        // This field should be false for all objects except during a
        // reachability traversal, where it is used intermittently to mark
        // objects which have already been visited in the traversal. Any code
        // doing such a traversal is responsibile for resetting all of the
        // `reached` flags to false afterwards.
        //
        // This should never be set on a free object.
    Object *next;
        // For allocated objects, this is the next pointer in a stack used for
        // depth-first traversal of the object pool during reachability
        // analysis.
        //
        // For free objects, which are never reached during depth-first
        // traversal, it is the next pointer in the free list.

    // Type-specific data.
    enum {
        OBJ_SYMBOL,
        OBJ_BLOCK,
        OBJ_GLOBAL,
        OBJ_FREE,
    } type;
    union {
        const Symbol *symbol;   // Symbol for symbol objects
        struct {
            const Symbol *tag;  // Class of blocks
            Expr *code;         // Body of blocks
        };
    };
};

// An ObjectPool is an allocator for Objects, optimized for fast creation,
// destruction, and reuse of short-lived objects, which are very common in bl:mp
// programs. See the comments in object.c for details on the implementation.
typedef struct {
    size_t batch_size;
    struct ObjectBatch *batches;
    Object *free_list;
    const Symbol *symbol_tag;
} ObjectPool;

PRIVATE Status ObjectPool_Init(Blimp *blimp, ObjectPool *pool);
PRIVATE void ObjectPool_Destroy(Blimp *blimp, ObjectPool *pool);
PRIVATE BlimpGCStatistics ObjectPool_GetStats(ObjectPool *pool, Object *root);
PRIVATE Status BlimpObject_NewGlobal(Blimp *blimp, Object **object);

#endif
