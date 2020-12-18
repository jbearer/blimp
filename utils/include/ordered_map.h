////////////////////////////////////////////////////////////////////////////////
// A generic ordered map
//
// This module provides a generic associative container interface, implemented
// as a red-black tree. It provides the standard O(log(n)) insertion, lookup,
// and removal. It also provides O(log(n)) min and max operations, and O(n) in-
// order traversal.
//

#ifndef BLIMP_ORDERED_MAP_H
#define BLIMP_ORDERED_MAP_H

#include "common.h"
#include "vector.h"

typedef int(*CmpFunc)(const void *key1, const void *key2);
    // Comparison function. Should return negative if `key1 < key2`, 0 if
    // `key1 == key2`, and positive if `key1 > key2`.

typedef struct {
    Blimp *blimp;
    size_t size;
    size_t key_size;
    size_t value_size;
    CmpFunc cmp;
    struct OrderedMapNode *root;
} OrderedMap;

typedef struct OrderedMapNode OrderedMapEntry;

typedef Vector/*<OrderedMapNode *>*/ OrderedMapIterator;

PRIVATE void OrderedMap_Init(
    Blimp *blimp,
    OrderedMap *map,
    size_t key_size,
    size_t value_size,
    CmpFunc cmp);
PRIVATE void OrderedMap_Destroy(OrderedMap *map);
PRIVATE void OrderedMap_Move(OrderedMap *from, OrderedMap *to);

PRIVATE Status OrderedMap_Emplace(
    OrderedMap *map, const void *key, OrderedMapEntry **entry, bool *created);
PRIVATE void OrderedMap_CommitEmplace(OrderedMap *map, OrderedMapEntry *entry);
PRIVATE void OrderedMap_AbortEmplace(OrderedMap *map, OrderedMapEntry *entry);

PRIVATE void OrderedMap_GetEntry(
    const OrderedMap *map,
    OrderedMapEntry *entry,
    void **key,
    void **value);

static inline void *OrderedMap_GetKey(
    const OrderedMap *map, OrderedMapEntry *entry)
{
    void *key;
    OrderedMap_GetEntry(map, entry, &key, NULL);
    return key;
}

static inline void *OrderedMap_GetValue(
    const OrderedMap *map, OrderedMapEntry *entry)
{
    void *value;
    OrderedMap_GetEntry(map, entry, NULL, &value);
    return value;
}

static inline Status OrderedMap_Update(
    OrderedMap *map, const void *key, const void *value)
{
    OrderedMapEntry *entry;
    if (OrderedMap_Emplace(map, key, &entry, NULL) != BLIMP_OK) {
        return Blimp_Reraise(map->blimp);
    }

    memcpy(OrderedMap_GetValue(map, entry), value, map->value_size);
    OrderedMap_CommitEmplace(map, entry);

    return BLIMP_OK;
}

PRIVATE OrderedMapEntry *OrderedMap_FindEntry(
    const OrderedMap *map, const void *key);

static inline void *OrderedMap_Find(const OrderedMap *map, const void *key)
{
    OrderedMapEntry *entry = OrderedMap_FindEntry(map, key);
    if (entry) {
        return OrderedMap_GetValue(map, entry);
    } else {
        return NULL;
    }
}

PRIVATE OrderedMapEntry *OrderedMap_MaxEntry(const OrderedMap *map);

static inline const void *OrderedMap_MaxKey(const OrderedMap *map)
{
    OrderedMapEntry *entry = OrderedMap_MaxEntry(map);
    if (entry) {
        return OrderedMap_GetKey(map, entry);
    } else {
        return NULL;
    }
}

PRIVATE bool OrderedMap_Remove(OrderedMap *map, const void *key, void *value);

PRIVATE Status OrderedMap_Iterator(
    const OrderedMap *map, OrderedMapIterator *it);
PRIVATE Status OrderedMap_RIterator(
    const OrderedMap *map, OrderedMapIterator *it);

/**
 * \brief Advance an iterator.
 *
 * The next (key, value) pair in the traversal represented by `it` will be
 * stored in `key` and `value`. The keys returned by successive calls to
 * OrderedMap_Next() are in ascending order according to the comparison function
 * used to create the map.
 *
 * \returns
 *      `true` if there are more elements in the traversal after this one,
 *      `false` otherwise. Once OrderedMap_Next() returns `false`, it should not
 *      be called again with the same iterator.
 */
PRIVATE bool OrderedMap_Next(
    const OrderedMap *map,
    OrderedMapIterator *it,
    const void **key,
    void **value);

PRIVATE bool OrderedMap_RNext(
    const OrderedMap *map,
    OrderedMapIterator *it,
    const void **key,
    void **value);

static inline size_t OrderedMap_Size(const OrderedMap *map)
{
    return map->size;
}

#endif
