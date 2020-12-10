#ifndef BLIMP_ORDERED_SET_H
#define BLIMP_ORDERED_SET_H

#include "ordered_map.h"

typedef OrderedMap OrderedSet;
typedef OrderedMapIterator OrderedSetIterator;

static inline void OrderedSet_Init(
    Blimp *blimp, OrderedSet *set, size_t elem_size, CmpFunc cmp)
{
    OrderedMap_Init(blimp, set, elem_size, 0, cmp);
}

static inline void OrderedSet_Destroy(OrderedSet *set)
{
    OrderedMap_Destroy(set);
}

static inline void OrderedSet_Move(OrderedSet *from, OrderedSet *to)
{
    OrderedMap_Move(from, to);
}

static inline Status OrderedSet_Insert(OrderedSet *set, const void *elem)
{
    return OrderedMap_Update(set, elem, NULL);
}

static inline Status OrderedSet_Iterator(
    const OrderedSet *set, OrderedSetIterator *it)
{
    return OrderedMap_Iterator(set, it);
}

static inline bool OrderedSet_Next(
    const OrderedSet *set, OrderedSetIterator *it, const void **elem)
{
    return OrderedMap_Next(set, it, elem, NULL);
}

static inline size_t OrderedSet_Size(const OrderedSet *set)
{
    return OrderedMap_Size(set);
}

#endif
