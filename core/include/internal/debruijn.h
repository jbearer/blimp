////////////////////////////////////////////////////////////////////////////////
// DeBruijn Map
//
// A data structure for converting between nested, scoped identifiers and
// DeBruijn indices.
//
// The data structure consists of a stack of identifiers, with the innermost
// bound name on top of the stack. The corresponding identifier can be recovered
// from a DeBruijn index efficiently by indexing backwards into the stack. The
// DeBruijn index of a given identifier can be obtained via a backwards linear
// search.
//
// Note that the term "identifier" is used somewhat loosely here to mean any
// piece of data which is bound in a particular scope (hence, why they are
// represented as void* in this interface). For example, it may literally be a
// name or symbol bound in a scope, but it may also be an actual parameter which
// was passed to a scope, so that DeBruijn indices in the scope can be evaluated
// to the value of the parameter.
//

#ifndef BLIMP_DEBRUIJN_H
#define BLIMP_DEBRUIJN_H

#include "internal/common.h"
#include "internal/error.h"

typedef struct {
    Blimp *blimp;

    // The stack is represented by a standard, exponentionally resizing vector,
    // which supports efficient indexing, the main operation of this data
    // structure.
    void **entries;
    size_t size;
    size_t capacity;
} DeBruijnMap;

/**
 * \brief Create an empty DeBruijnMap.
 */
PRIVATE void DBMap_Init(Blimp *blimp, DeBruijnMap *map);

/**
 * \brief Destroy a DeBruijnMap, releasing any resources that it owns.
 */
PRIVATE void DBMap_Destroy(DeBruijnMap *map);

/**
 * \brief Add an identifier for a new nested scope.
 */
PRIVATE Status DBMap_Push(DeBruijnMap *map, void *identifier);

/**
 * \brief Remove the identifier for the innermost scope currently on the stack.
 *
 * \pre `map` must not be empty.
 * \returns The identifier which was removed.
 */
PRIVATE void *DBMap_Pop(DeBruijnMap *map);

/**
 * \brief Insert a new identifier for an outer scope.
 *
 * `identifier` is bound to a scope which contains the outermost scope currently
 * in `map`. In terms of the stack data structure, this corresponds to inserting
 * a new element at the bottom of the stack, and shifting all existing elements
 * up by one position.
 *
 * \note This operation has linear time complexity.
 */
PRIVATE Status DBMap_Shift(DeBruijnMap *map, void *identifier);

/**
 * \brief Resolve a DeBruijn index to its corresponding identifier.
 *
 * \pre `index` is less than the number of nested scopes currently on the stack.
 */
PRIVATE void *DBMap_Resolve(const DeBruijnMap *map, size_t index);

/**
 * \brief Get the DeBruijn index of a given identifier.
 *
 * This function returns in `index` the DeBruijn index of the innermost
 * identifier currently in scope which compares equal to `identifier` according
 * to the comparison function `eq`. If there is no matching identifier, an error
 * status is returned.
 *
 * \note This operation has linear time complexity.
 */
PRIVATE Status DBMap_Index(
    const DeBruijnMap *map,
    void *identifier,
    bool(*eq)(void *, void *),
    size_t *index);

/**
 * \brief Add a stack of nested scopes to `dst`.
 *
 * The stack of scopes represented by `src` is added, in order, inside the
 * innermost scope in `dst`, as if by calling DBMap_Push() repeatedly with each
 * scope in `src`.
 *
 * \note This operation has linear time complexity.
 */
PRIVATE Status DBMap_Append(DeBruijnMap *dst, const DeBruijnMap *src);

static inline bool DBMap_Empty(const DeBruijnMap *map)
{
    return map->size == 0;
}

static inline void DBMap_Clear(DeBruijnMap *map)
{
    map->size = 0;
}

#endif
