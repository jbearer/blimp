////////////////////////////////////////////////////////////////////////////////
// A generic hash map
//
// This module provides a generic set interface, implemented
// as a hash set. It provides the standard O(1) insertion, lookup, and removal.
//
// It is implemented as a thin wrapper around the HashMap type, where the
// elements of the set are the keys of the map, and the values of the map have
// zero size.


#ifndef BLIMP_HASH_SET_H
#define BLIMP_HASH_SET_H

#include "hash_map.h"

typedef HashMapOptions HashSetOptions;
typedef HashMapEntry HashSetEntry;
typedef HashMap HashSet;

static inline Status HashSet_Init(
    Blimp *blimp,
    HashSet *set,
    size_t elem_size,
    EqFunc eq_func,
    HashFunc hash_func,
    const HashSetOptions *options)
{
    return HashMap_Init(blimp, set, elem_size, 0, eq_func, hash_func, options);
}

static inline void HashSet_Destroy(HashSet *set)
{
    HashMap_Destroy(set);
}

static inline size_t HashSet_Size(const HashSet *set)
{
    return HashMap_Size(set);
}

static inline bool HashSet_Empty(const HashSet *set)
{
    return HashMap_Empty(set);
}

static inline HashSetEntry *HashSet_Begin(const HashSet *set)
{
    return HashMap_Begin(set);
}

static inline HashSetEntry *HashSet_End(const HashSet *set)
{
    return HashMap_End(set);
}

static inline HashSetEntry *HashSet_Next(const HashSet *set, HashSetEntry *it)
{
    return HashMap_Next(set, it);
}

static inline void *HashSet_GetEntry(const HashSet *set, HashSetEntry *it)
{
    return HashMap_GetKey(set, it);
}

static inline bool HashSet_Contains(const HashSet *set, const void *elem)
{
    return HashMap_Find(set, elem) != NULL;
}

static inline Status HashSet_Insert(HashSet *set, const void *elem)
{
    return HashMap_Update(set, elem, NULL);
}

static inline Status HashSet_FindOrInsert(
    HashSet *set, const void *elem, bool *found)
{
    void *value = NULL;
    TRY(HashMap_FindOrInsert(set, elem, NULL, &value));
    *found = value != NULL;
    return BLIMP_OK;
}

static inline Status HashSet_Union(HashSet *s1, const HashSet *s2)
{
    return HashMap_Union(s1, s2);
}

static inline void HashSet_Clear(HashSet *set)
{
    HashMap_Clear(set);
}

#endif
