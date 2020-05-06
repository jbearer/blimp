////////////////////////////////////////////////////////////////////////////////
// A generic hash map
//
// This module provides a generic associative container interface, implemented
// as a hash map. It provides the standard O(1) insertion, lookup, and removal.
//
// Naturally, the interface is not at all typesafe, and there are a number of
// pitfalls clients can encounter. For example, many of the metohds take a
// pointer to the key type of the map. In the common case where the key type is
// itself a pointer type, it is very easy to accidentally pass a key by value,
// rather than passing a pointer to the key.
//
// As such, the recommended way to use this module is to keep the amount of
// code which directly interfaces with the hash map to the absolute minimum
// typesafe wrapper, and to implement most of your code in terms of the relevant
// types, rather than `void *`.

#ifndef BLIMP_HASH_MAP_H
#define BLIMP_HASH_MAP_H

#include <string.h>

#include "internal/common.h"
#include "internal/error.h"

typedef struct HashMapEntry HashMapEntry;

typedef size_t (*HashFunc)(const void *key, void *arg);
typedef bool (*EqFunc)(const void *key1, const void *key2, void *arg);

// Options controlling how hash maps are created. You may optionally pass a
// pointer to a HashMapOptions struct as the last argument of HashMap_Init to
// customize various properties of the table. The recommended way to set options
// is to copy HASH_MAP_DEFAULT_OPTIONS, modify the fields of interest, and pass
// a pointer to the copy to HashMap_Init. This technique only requires
// explicitly setting the fields you want to change, and it should be forwards
// compatible with new options being added.
typedef struct {
    size_t log2_initial_capacity;
        // Controls the size of the initial dynamic allocation for the map
        // data. The capacity of the map must be a power of two, so the initial
        // capacity of the map will be `n` = `2 ^ log2_initial_capacity`.
        //
        // This means that the map, when first created, will use space
        // proportional to `n` and to the size of the key and value type. It
        // does not mean that the map can hold `n` entries before resizing. For
        // performance reasons, the map will automatically grow when a fixed
        // fraction of `n` entries have been added.
        //
        // The default is 3, for a capacity of 8 entries.

    bool create_empty;
        // If set, no memory will be allocated for the map when it is first
        // created. Only when an entry is first added will we allocated space
        // proportional to `2 ^ log2_initial_capacity`.
        //
        // The default is false.

    void *user_data;
        // Arbitrary pointer-sized data which will be passed back into the hash
        // and equality functions.
        //
        // The default is NULL.
} HashMapOptions;

extern const HashMapOptions HASH_MAP_DEFAULT_OPTIONS;

// The hash map data type. This should be treated as an opaque struct and only
// accessed using the methods provided here.
typedef struct {
    HashMapEntry *entries;
        // Note that this isn't _really_ an array of HashMapEntry objects; since
        // HashMapEntry is a variably sized struct consisting of a header
        // followed by the key and value, C's built-in array indexing math won't
        // be correct.
    HashMapEntry *first;
    HashMapEntry *last;
    size_t capacity;
    size_t size;
    size_t key_size;
    size_t value_size;
    Blimp *blimp;
    HashFunc hash;
    EqFunc eq;
    void *user_data;
} HashMap;

/**
 * \brief Initialize an empty hash map.
 *
 * \param blimp
 *      A Blimp interpreter. This is only used for memory allocation and error
 *      handling.
 * \param map
 *      The map to initialize.
 * \param key_size
 *      The size in bytes of the key type for the map.
 * \param value_size
 *      The size in bytes of the value type for the map.
 * \param eq_func
 *      An equality function on keys. All of the standard properties of an
 *      equivalence relation should hold (reflexivity, symmetry, and
 *      transitivity).
 * \param hash_func
 *      A hash function for keys. The only requirement of this function is that
 *      it returns the same output for all pairs of equivalent inputs (with
 *      respect to `eq_func`). However, the choice of hash function can
 *      drastically affect hash map performance. This function should ideally
 *      satisfy the following requirements:
 *          * It should be efficient to compute.
 *          * It should approximate a uniform distribution over the space of
 *            64-bit hashes.
 * \param options
 *      A pointer to HashMapOptions object controlling various properties of the
 *      map. Passing NULL here is the same as passing
 *      `&HASH_MAP_DEFAULT_OPTIONS`.
 */
PRIVATE Status HashMap_Init(
    Blimp *blimp,
    HashMap *map,
    size_t key_size,
    size_t value_size,
    EqFunc eq_func,
    HashFunc hash_func,
    const HashMapOptions *options);

PRIVATE void HashMap_Destroy(HashMap *map);

static inline size_t HashMap_Size(const HashMap *map)
{
    return map->size;
}

static inline Blimp *HashMap_GetBlimp(const HashMap *map)
{
    return map->blimp;
}

static inline HashMapEntry *HashMap_Begin(const HashMap *map)
{
    return map->entries;
}

PRIVATE HashMapEntry *HashMap_Next(const HashMap *map, HashMapEntry *it);

static inline HashMapEntry *HashMap_End(const HashMap *map)
{
    (void)map;
    return NULL;
}

/**
 * \brief Low-level insertion and updates.
 *
 * \param[in]   map     the map to insert into
 * \param[in]   key     a pointer to the key indicating where to insert
 * \param[out]  entry   a reference to the entry in `map` with key `key`
 * \param[out]  created whether `entry` was newly created
 *
 * HashMap_Emplace allows the user of the HashMap to construct an entry in place
 * in the memory owned by the HashMap where it will ultimately live, or to
 * update an existing entry in place.
 *
 * It takes as input a key to insert or look up in the map. If an entry with
 * that key already exists, it returns a reference to that entry. Otherwise, it
 * inserts a new entry into the map with that key and an uninitalized value, and
 * it returns a reference to the new entry.
 *
 * After calling HashMap_Emplace, the caller should check `created`. If it is
 * set, it is the caller's responsibility to initialize the value of the new
 * entry (and optionally tweak the key, without changing the hash). You can use
 * HashMap_GetEntry to get pointers to the fields of the entry.
 *
 * If `created` is not set, the caller may leave well enough alone, or they may
 * optionally update the key and value of the existing entry in place. Any
 * updates made to the key must not change the hash or equivalence class of the
 * key.
 *
 * When the caller is satisfied with the state of `entry`, they must finish
 * their transaction and commit the new entry to the map persistently by calling
 * HashMap_CommitEmplace. If instead they discover that they don't want the new
 * entry to be inserted, they must call HashMap_AbortEmplace, which removes the
 * newly created element from the map.
 *
 * This somewhat complex mechanism allows the caller to avoid a potential
 * performance problem when creating a new element is expensive: they would like
 * to create their new element only if they actually need to insert it. The
 * alternative is calling Find and then creating the value and calling Insert if
 * Find fails. However, this requires two lookup operations instead of one.
 * Emplace solves this problem by retaining a reference to the location of the
 * object after the first lookup.
 *
 * Because of this Emplace may be the most efficient way to perform an
 * insert-if-not-present type of operation. However, it is probably not the most
 * legible. If you just want to insert or update a value regardless of whether
 * the key already exists, you should prefer HashMap_Update. If performance is
 * not a concern, you can emulate the behavior of HashMap_Emplace more legibly
 * using a combination of HashMap_Find and HashMap_Update.
 *
 * \note
 *      You must not call other HashMap functions in between calling
 *      HashMap_Emplace and either HashMap_CommitEmplace or
 *      HashMap_AbortEmplace.
 */
PRIVATE Status HashMap_Emplace(
    HashMap *map, const void *key, HashMapEntry **entry, bool *created);
PRIVATE void HashMap_CommitEmplace(HashMap *map, HashMapEntry *entry);
PRIVATE void HashMap_AbortEmplace(HashMap *map, HashMapEntry *entry);

/**
 * \brief Get references to the key and value of a map entry.
 *
 * After calling this function, `*key` will point to the key field inside of
 * `entry`, and `*value` will point to the value field. Note that these return
 * parameters alias `entry`: writing through them will write to the data that is
 * stored in the map.
 *
 * It is permissible to write through `key`, but you must not change the
 * equivalence class of `**key`, or else you may violate the internal
 * consistency constraints of the map. This is not checked. You may do whatever
 * you want with `value`.
 */
PRIVATE void HashMap_GetEntry(
    const HashMap *map, HashMapEntry *entry, void **key, void **value);

static inline void *HashMap_GetKey(const HashMap *map, HashMapEntry *entry)
{
    void *key;
    HashMap_GetEntry(map, entry, &key, NULL);
    return key;
}

static inline void *HashMap_GetValue(const HashMap *map, HashMapEntry *entry)
{
    void *value;
    HashMap_GetEntry(map, entry, NULL, &value);
    return value;
}

/**
 * \brief Associate a new value with key.
 *
 * If `*key` does not already exist in the map, a new entry will be create. If
 * it does exist, the value there will be replaced by `*value`.
 */
static inline Status HashMap_Update(
    HashMap *map, const void *key, const void *value)
{
    HashMapEntry *entry;
    TRY(HashMap_Emplace(map, key, &entry, NULL));
    memcpy(HashMap_GetValue(map, entry), value, map->value_size);
    HashMap_CommitEmplace(map, entry);
    return BLIMP_OK;
}

/**
 * \brief Get a reference to the entry with the given key.
 *
 * This can be an efficient way to update the value corresponding to a key only
 * if the key already exists in the map, without making separate calls to
 * HashMap_Find and HashMap_Update.
 *
 * If the given key does not exist in the map, the result is `NULL`.
 */
PRIVATE HashMapEntry *HashMap_FindEntry(const HashMap *map, const void *key);

/**
 * \brief Get a pointer to the value associated with `key`.
 *
 * If `key` does not exist in the map, the result is NULL.
 */
static inline void *HashMap_Find(const HashMap *map, const void *key)
{
    HashMapEntry *entry = HashMap_FindEntry(map, key);
    if (entry) {
        return HashMap_GetValue(map, entry);
    } else {
        return NULL;
    }
}

/**
 * \brief Remove an entry from the map given its key.
 *
 * If `*key` exists in the map, it's corresponing entry is removed. If `value`
 * is not `NULL`, it must point to a contiguously allocated memory region of at
 * least `value_size` bytes, where the value which was removed will be copied.
 *
 * The result is `true` if an entry was found and removed, or `false` if no
 * entry with the given key was found. If the result is `false`, the contents of
 * `value` are undefined.
 */
PRIVATE bool HashMap_Remove(HashMap *map, const void *key, void *value);

PRIVATE void HashMap_Clear(HashMap *map);

#endif
