#include "internal/debruijn.h"

void DBMap_Init(Blimp *blimp, DeBruijnMap *map)
{
    map->blimp = blimp;
    map->size = 0;
    map->capacity = 0;
    map->entries = NULL;
}

void DBMap_Destroy(DeBruijnMap *map)
{
    Free(map->blimp, &map->entries);
}

Status DBMap_Push(DeBruijnMap *map, void *value)
{
    // Increase the capacity if necessary.
    if (map->size >= map->capacity) {
        map->capacity = 2*map->capacity + 1;
            // We double the capacity and add one to cleanly handle the case
            // where capacity is 0.
        TRY(Realloc(map->blimp, map->capacity*sizeof(void *), &map->entries));
    }

    map->entries[map->size++] = value;
    return BLIMP_OK;
}

Status DBMap_Shift(DeBruijnMap *map, void *value)
{
    // Increase the capacity if necessary.
    if (map->size >= map->capacity) {
        map->capacity = 2*map->capacity + 1;
            // We double the capacity and add one to cleanly handle the case
            // where capacity is 0.
        TRY(Realloc(map->blimp, map->capacity*sizeof(void *), &map->entries));
    }

    memmove(&map->entries[1], &map->entries[0], map->size*sizeof(void *));
        // Shift all the existing entries over by 1.

    map->entries[0] = value;
        // Add the new entry at the bottom of the stack.

    ++map->size;
    return BLIMP_OK;
}

void *DBMap_Pop(DeBruijnMap *map)
{
    assert(map->size > 0);
    return map->entries[--map->size];
}

void *DBMap_Resolve(const DeBruijnMap *map, size_t index)
{
    assert(index < map->size);
    return map->entries[map->size - index - 1];
}

Status DBMap_Index(
    const DeBruijnMap *map,
    void *value,
    bool(*eq)(void *, void *),
    size_t *index)
{
    for (*index = 0; *index < map->size; ++*index) {
        if (eq(value, DBMap_Resolve(map, *index))) {
            return BLIMP_OK;
        }
    }

    return Error(map->blimp, BLIMP_ERROR);
}

Status DBMap_Append(DeBruijnMap *dst, const DeBruijnMap *src)
{
    // Increase the capacity if necessary.
    if (dst->size + src->size > dst->capacity) {
        // Since we know exactly how much space we need, and this operation is
        // not amortized constant time anyways, we don't need to worry about
        // growing exponentionally. Allocate just enough space.
        dst->capacity = dst->size + src->size;
        TRY(Realloc(dst->blimp, dst->capacity*sizeof(void *), &dst->entries));
    }

    memcpy(&dst->entries[dst->size], src->entries, src->size*sizeof(void *));
    dst->size += src->size;
    return BLIMP_OK;
}
