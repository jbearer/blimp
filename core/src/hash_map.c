////////////////////////////////////////////////////////////////////////////////
// Hash map implementation details
//
// This hash map is implemented using open addressing with quadratic probing for
// collision resolution.
//
// Open addressing was chosen because, compared to the alternative (separate
// chaining) it has better locality since the map data is contiguously
// allocated, and it does not have the space overhead of storing a linked list
// for each bucket.
//
// Quadratic probing was chosen because it has better theoretical behavior in
// the presence of hash collisions than linear probing, and it is easier to
// implement than double hashing, which requires a second hash function, and a
// prime-sized table. Quadratic probing requires a power-of-two sized table,
// which is much easier to create and is probably friendlier to the allocator.
//

#include <assert.h>
#include <limits.h>
#include <string.h>

#include "internal/blimp.h"
#include "internal/error.h"
#include "internal/hash_map.h"

// The maximum load factor: once the map is more than MAX_LOAD_NUM/MAX_LOAD_DEN
// full, we increase its capacity by a factor of two.
#define MAX_LOAD_NUM 3
#define MAX_LOAD_DEN 4  // Must be a power of two.

#define ALIGNMENT (sizeof(size_t))
    // Must be a power of two.

struct HashMapEntry {
    size_t hash;
    enum {
        ABSENT = 0,
            // This entry has never contained valid data. It can be used to
            // insert a new item. If we reach an ABSENT entry during a search,
            // we know the key we're searching for does not exist, because if it
            // did it would be stored in that entry.
        DELETED,
            // This entry used to store valid data, but it was deleted from the
            // map. It can be used to insert a new item. If we reach a DELETED
            // entry during a search, we have to continue searching. The key
            // we're looking for might exist in the table, if this entry was
            // occupied when the key was first inserted and was then deleted
            // later.
        PRESENT,
            // This entry contains a valid key-value pair.
        CREATED,
            // This entry has been allocated for a particular key, but it is
            // still being initialized after a HashMap_Emplace. It will be fully
            // added to the table (transition to the PRESENT state) after
            // HashMap_CommitEmplace is called, or it will be reemoved
            // (transition to the DELETED state) if HashMap_AbortEmplace is
            // called.
    } status;

    char key[] __attribute__((aligned (ALIGNMENT)));
    // Value is at offsetof(HashMapEntry, key) + RoundUpToAlignment(key_size).
};

PRIVATE const HashMapOptions HASH_MAP_DEFAULT_OPTIONS = {
    .log2_initial_capacity = 3,
    .create_empty          = false,
    .user_data             = NULL,
};

static inline size_t RoundUpToAlignment(size_t n)
{
    return (n + (ALIGNMENT - 1)) & ~(ALIGNMENT - 1);
}

// Because the HashMapEntry structure is dynamically sized based on the
// `key_size` and `value_size` that the user gives us, we have to do our own
// size computation...
static inline size_t EntrySize(const HashMap *map)
{
    return sizeof(HashMapEntry)
         + RoundUpToAlignment(map->key_size)
         + RoundUpToAlignment(map->value_size);
    ;
}
// ...and our own array indexing.
static inline HashMapEntry *NthEntry(HashMap *map, size_t n)
{
    assert(n < map->capacity);
    return (HashMapEntry *)((char *)map->entries + n*EntrySize(map));
}

// Get a reference to the entry containing the given `key`, or the entry where
// the key should go if it does not already exist in the map.
//
// This function will terminate as long as the key exists in the map, _or_ there
// is at least one ABSENT entry in the map. The MAX_LOAD constraint should
// guarantee the latter condition for all valid maps.
static HashMapEntry *Find(HashMap *map, size_t hash, const void *key)
{
    size_t n = hash % map->capacity;
        // The index of the entry we're currently searching.
    HashMapEntry *ret = NULL;
        // If we ever encounter a DELETED entry in our search, and then later we
        // fail to find the key, we want to return the DELETED entry so it can
        // be reclaimed to insert the new key. If we have ever encountered such
        // a node during the search, it will be stored here so we can return it
        // at the end.
    size_t probe_delta = 1;
        // If we have a collision, we probe using triangular numbers; we try
        // entry `n + 1`, then `n + 3`, then `n + 6`, and so on. There are two
        // advantages to this sequence:
        //  * It is easy to compute incrementally: `probe_delta`, the difference
        //    between successive items in the sequence, simply increases by 1
        //    for each item.
        //  * Every integer in [0, 2^m - 1] is a residue of a triangular number
        //    mod 2^m. What this means is that as long as the capacity of our
        //    map is a power of two, our probing will eventually reach every
        //    entry in the map, so as long as the key exists in the map or there
        //    is at least one empty entry, we are guaranteed to find the key or
        //    the place where the key should go.

    assert(map->size < map->capacity);
    while (true) {
        HashMapEntry *entry = NthEntry(map, n);
        switch (entry->status) {
            case PRESENT:
                // The entry contains data, check if its key is the one we're
                // looking for.
                if (hash == entry->hash &&
                    map->eq(key, entry->key, map->user_data))
                {
                    return entry;
                } else {
                    // If not, we have a collision. Keep looking.
                    n = (n + probe_delta) % map->capacity;
                    ++probe_delta;
                }
                break;

            case DELETED:
                if (ret == NULL) {
                    ret = entry;
                        // Save the DELETED entry so we can reclaim it if we
                        // don't end up finding a PRESENT entry that contains
                        // our key.
                }

                // Keep looking in case the key does exist in the table.
                n = (n + probe_delta) % map->capacity;
                ++probe_delta;
                break;

            case ABSENT:
                return ret ? ret : entry;
                    // If we've reached an ABSENT node and we haven't found our
                    // key, it doesn't exist.

            default:
                assert(false);
        }
    }
}

// Make space for one more entry, resizing the map if necessary.
static Status MakeSpace(HashMap *map)
{
    if (map->entries && MAX_LOAD_NUM*map->capacity > MAX_LOAD_DEN*map->size) {
        // We already have sufficient capacity to accomodate more than `size`
        // entries, and `entries` is non-NULL, meaning we actually have
        // `capacity` entries worth of space. Note that both checks are
        // important: due to lazy allocation (HashMapOptions::create_empty) it
        // is possible that `capacity` is sufficiently large even when `entries`
        // is NULL.
        //
        // Since we already have enough space, don't resize the table.
        return BLIMP_OK;
    }

    if (map->entries == NULL) {
        // This is not actually a resize but the first allocation of a table
        // that was created with `create_empty` set. This means we don't have to
        // increase the table capacity, we just have to allocate that much
        // space. We also don't have to worry about rehashing and copying over
        // old data, since there isn't any.
        return Calloc(map->blimp, map->capacity, EntrySize(map), &map->entries);
            // Calloc ensures that all the entries in the new array are zero-
            // initialized, which means that their `status` field is ABSENT.
    }

    size_t        old_capacity = map->capacity;
    HashMapEntry *old_entries  = map->entries;

    size_t new_capacity = old_capacity * 2;
    HashMapEntry *new_entries;
    TRY(Calloc(map->blimp, new_capacity, EntrySize(map), &new_entries));
        // Calloc ensures that all the entries in the new array are zero-
        // initialized, which means that their `status` field is ABSENT.

    map->capacity = new_capacity;
    map->entries  = new_entries;

    // For each entry in the old map, compute it's position in the new map and
    // copy it there.
    for (size_t i = 0; i < old_capacity; ++i) {
        HashMapEntry *old_entry =
            (HashMapEntry *)((char *)old_entries + i*EntrySize(map));
        if (old_entry->status != PRESENT) {
            continue;
        }

        // Search for the location in the new map where this entry should go.
        // This loop implements a quadratic probing search using triangular
        // numbers, very similar to Find. However, it can be somewhat simpler
        // than Find, since:
        //  * We only have to worry about PRESENT and ABSENT entries. DELETED
        //    entries get skipped in the outer loop, so we never copy a DELETED
        //    entry into the new map.
        //  * We know all the keys in the old map are distinct, so we never have
        //    to do any equality checks in this loop: if we find a PRESENT node,
        //    we know it is not equal to the key we're currently working on, so
        //    we just keep probing.
        // With those two simplification, this loop amounts to just finding the
        // first ABSENT node on the triangular number sequence and copying the
        // old node there.
        size_t n           = old_entry->hash % new_capacity;
        size_t probe_delta = 1;
        while (true) {
            HashMapEntry *new_entry = NthEntry(map, n);
            if (new_entry->status == ABSENT) {
                memcpy(new_entry, old_entry, EntrySize(map));
                break;
            }
            n = (n + probe_delta) % new_capacity;
            ++probe_delta;
        }
    }

    return BLIMP_OK;
}

Status HashMap_Init(
    Blimp *blimp,
    HashMap *map,
    size_t key_size,
    size_t value_size,
    EqFunc eq_func,
    HashFunc hash_func,
    const HashMapOptions *options)
{
    if (options == NULL) {
        options = &HASH_MAP_DEFAULT_OPTIONS;
    }

    map->blimp      = blimp;
    map->key_size   = key_size;
    map->value_size = value_size;
    map->hash       = hash_func;
    map->eq         = eq_func;
    map->user_data  = options->user_data;

    map->size = 0;

    assert(options->log2_initial_capacity < sizeof(unsigned long long)*CHAR_BIT);
    map->capacity = 1ull << options->log2_initial_capacity;
    if (map->capacity < MAX_LOAD_DEN) {
        map->capacity = MAX_LOAD_DEN;
    }

    if (options->create_empty) {
        map->entries = NULL;
    } else {
        TRY(Calloc(blimp, map->capacity, EntrySize(map), &map->entries));
    }

    return BLIMP_OK;
}

void HashMap_Destroy(HashMap *map)
{
    Free(map->blimp, &map->entries);
}

Status HashMap_Emplace(
    HashMap *map, const void *key, HashMapEntry **entry, bool *created)
{
    TRY(MakeSpace(map));

    size_t hash = map->hash(key, map->user_data);
    *entry = Find(map, hash, key);
    if ((*entry)->status == PRESENT) {
        *created = false;
    } else {
        memcpy((*entry)->key, key, map->key_size);
        (*entry)->hash = hash;
        (*entry)->status = CREATED;
        *created = true;
    }

    return BLIMP_OK;
}

void HashMap_CommitEmplace(HashMap *map, HashMapEntry *entry)
{
    assert(map->hash(entry->key, map->user_data) == entry->hash);
    if (entry->status == CREATED) {
        entry->status = PRESENT;
        ++map->size;
    } else {
        assert(entry->status == PRESENT);
    }
}

void HashMap_AbortEmplace(HashMap *map, HashMapEntry *entry)
{
    (void)map;

    if (entry->status == CREATED) {
        entry->status = DELETED;
    } else {
        assert(entry->status == PRESENT);
    }
}

void *HashMap_Find(HashMap *map, const void *key)
{
    HashMapEntry *entry = Find(map, map->hash(key, map->user_data), key);
    if (entry->status == PRESENT) {
        return HashMap_GetValue(map, entry);
    } else {
        return NULL;
    }
}

bool HashMap_Remove(HashMap *map, const void *key, void *value)
{
    HashMapEntry *entry = Find(map, map->hash(key, map->user_data), key);
    if (entry->status == PRESENT) {
        if (value) {
            memcpy(value, HashMap_GetValue(map, entry), map->value_size);
        }
        entry->status = DELETED;
        return true;
    } else {
        return false;
    }
}

void HashMap_GetEntry(
    HashMap *map, HashMapEntry *entry, void **key, void **value)
{
    if (key) {
        *key = &entry->key;
    }
    if (value) {
        *value = (char *)&entry->key + RoundUpToAlignment(map->key_size);
    }
}
