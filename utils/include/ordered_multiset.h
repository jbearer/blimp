////////////////////////////////////////////////////////////////////////////////
// A multiset whose elements are stored in order.
//
// This set is implemented as a thin wrapper around OrderedMap, mapping keys to
// counts.
///

#ifndef BLIMP_ORDERED_MULTISET_H
#define BLIMP_ORDERED_MULTISET_H

#include "ordered_map.h"

typedef OrderedMap OrderedMultiset;

static inline void OrderedMultiset_Init(
    Blimp *blimp, OrderedMultiset *set, size_t elem_size, CmpFunc cmp)
{
    OrderedMap_Init(blimp, set, elem_size, sizeof(size_t), cmp);
}

static inline void OrderedMultiset_Destroy(OrderedMultiset *set)
{
    OrderedMap_Destroy(set);
}

static inline Status OrderedMultiset_Insert(
    OrderedMultiset *set, const void *elem)
{
    OrderedMapEntry *entry;
    bool created;
    TRY(OrderedMap_Emplace(set, elem, &entry, &created));

    size_t *count = OrderedMap_GetValue(set, entry);
    if (created) {
        // If this is the first time this element has been added to the set,
        // it's count is 1.
        *count = 1;
    } else {
        // Otherwise, increment the existing count.
        ++*count;
    }
    OrderedMap_CommitEmplace(set, entry);

    return BLIMP_OK;
}

static inline void OrderedMultiset_Remove(
    OrderedMultiset *set, const void *elem)
{
    size_t *count = OrderedMap_Find(set, elem);
    if (count != NULL) {
        assert(*count > 0);
        if (--*count == 0) {
            OrderedMap_Remove(set, elem, NULL);
        }
    }
}

static inline const void *OrderedMultiset_Max(OrderedMultiset *set)
{
    return OrderedMap_MaxKey(set);
}

#endif
