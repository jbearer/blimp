#include <assert.h>
#include <stddef.h>

#include "internal/blimp.h"
#include "internal/expr.h"
#include "internal/error.h"
#include "internal/symbol.h"

typedef struct {
    const char *data;
    size_t length;
} String;

static size_t StringHash(const String *str, void *arg)
{
    (void)arg;

    size_t hash = HASH_SEED;
    Hash_AddBytes(&hash, str->data, str->length);
    return hash;
}

static bool StringEq(const String *str1, const String *str2, void *arg)
{
    (void)arg;
    return str1->length == str2->length
        && memcmp(str1->data, str2->data, str1->length) == 0;
}

PRIVATE size_t SymbolHash(const Symbol **symbol, void *arg)
{
    (void)arg;
    return (*symbol)->hash;
}

bool SymbolEq(const Symbol **sym1, const Symbol **sym2, void *arg)
{
    (void)arg;
    return *sym1 == *sym2;
}

Status SymbolTable_Init(Blimp *blimp, SymbolTable *symbols)
{
    return HashMap_Init(
        blimp, symbols, sizeof(String), sizeof(Symbol *),
        (EqFunc)StringEq, (HashFunc)StringHash, NULL);
}

void SymbolTable_Destroy(SymbolTable *symbols)
{
    Blimp *blimp = HashMap_GetBlimp(symbols);

    for (HashMapEntry *entry = HashMap_Begin(symbols);
         entry != HashMap_End(symbols);
         entry = HashMap_Next(symbols, entry))
    {
        Free(blimp, (char **)HashMap_GetKey(symbols, entry));
        Free(blimp, (Symbol **)HashMap_GetValue(symbols, entry));
    }

    HashMap_Destroy(symbols);
}

Status SymbolTable_GetSymbol(
    SymbolTable *symbols,
    const char *name,
    size_t length,
    const Symbol **symbol)
{
    Blimp *blimp = HashMap_GetBlimp(symbols);

    HashMapEntry *entry;
    bool created;
    TRY(HashMap_Emplace(symbols, &(String){name, length}, &entry, &created));

    String *key;
    Symbol **value;
    size_t hash;
    HashMap_GetEntry(symbols, entry, (void **)&key, (void **)&value, &hash);

    if (!created) {
        // The entry we are using was already in the map, so it's fully valid,
        // and there's nothing else for us to do.
        *symbol = *value;
        return BLIMP_OK;
    }

    // Otherwise, we created and inserted a new entry into the map. Now we need
    // to initialie it. It's key is currently the same pointer as `name`:
    assert(key->data == name);
    // In order to take ownership of that string, we need to duplicate it into
    // our own memory:
    Status ret;
    if ((ret = Strndup(blimp, name, length, (char **)&key->data)) != BLIMP_OK) {
        HashMap_AbortEmplace(symbols, entry);
        return ret;
    }
    // The value of the entry is completely uninitialized. We need to create a
    // new symbol for it:
    Symbol *new_symbol;
    if ((ret = Malloc(blimp, sizeof(Symbol), &new_symbol)) != BLIMP_OK) {
        HashMap_AbortEmplace(symbols, entry);
        return ret;
    }
    Object_Init((Object *)new_symbol, blimp, OBJ_SYMBOL);
    new_symbol->length = length;
    new_symbol->name   = key->data;
    new_symbol->hash   = hash;
    *value  = new_symbol;
    *symbol = new_symbol;

    HashMap_CommitEmplace(symbols, entry);
    return BLIMP_OK;
}

const char *BlimpSymbol_GetName(const Symbol *sym)
{
    return sym->name;
}

size_t BlimpSymbol_Hash(const Symbol *sym)
{
    return sym->hash;
}

#define MALLOC_ALIGN_BITS ( \
    _Alignof(max_align_t) == 4 ? (size_t)2 : \
    _Alignof(max_align_t) == 8 ? (size_t)3 : \
    _Alignof(max_align_t) == 16 ? (size_t)4 : \
        (size_t)-1 \
)
_Static_assert(MALLOC_ALIGN_BITS != (size_t)-1,
    "unable to determine malloc alignment");

#define SIZEOF_FIELD(TYPE, FIELD) sizeof(((TYPE *)NULL)->FIELD)

#define SYMBOL_TAG_INVALID ((uintptr_t)-1)
    // An invalid or not-present symbol is represented with a tag consisting of
    // all 1's. This is guaranteed to conflict with any tag or partial tag,
    // since tags are always smaller than a uintptr_t (the alignment bits are
    // shifted off first thing) and the extra bits are 0.
#define SYMBOL_MAP_FANOUT ((uintptr_t)1 << SYMBOL_MAP_LOG_FANOUT)
#define SYMBOL_MAP_INDEX_MASK (SYMBOL_MAP_FANOUT - 1)
    // A bitmask for the next chunk of bits in a tag.
#define SYMBOL_MAP_TAG_SHIFT MALLOC_ALIGN_BITS
    // We ignore the lowest few bits in a symbol address when constructing its
    // tag, since these bits are always 0 due to alignment and thus would lead
    // to frequent collisions at the first level of the tree.
_Static_assert(
    SYMBOL_MAP_FANOUT <= SIZEOF_FIELD(SymbolNode, sub_trees)*CHAR_BIT,
    "symbol map fanout is too large for allocated sub-trees bitmap");
_Static_assert(
    SYMBOL_MAP_LOG_FANOUT < SIZEOF_FIELD(SymbolNodeEntry, to_next)*CHAR_BIT,
    "SymbolNodeEntry::to_next is too small to represent both positive and "
    "negative offsets of up to SYMBOL_MAP_FANOUT");
_Static_assert(
    SYMBOL_MAP_LOG_FANOUT <= SIZEOF_FIELD(SymbolNode, first)*CHAR_BIT,
    "SymbolNode::first is too small to represent indices up to "
    "SYMBOL_MAP_FANOUT");

static void SymbolMapAllocator_Free(SymbolMapAllocator *alloc, SymbolNode *node)
{
    assert(node != NULL);
    node->next = alloc->free_nodes;
    alloc->free_nodes = node;
}

static Status SymbolMapAllocator_Alloc(
    SymbolMapAllocator *alloc, SymbolNode **node)
{
    if (alloc->free_nodes == NULL) {
        TRY(Malloc(alloc->blimp,
            sizeof(SymbolNode) +
            SYMBOL_MAP_FANOUT*sizeof(SymbolNodeEntry), node));
        (*node)->sub_trees = 0;
    } else {
        *node = alloc->free_nodes;
        alloc->free_nodes = (*node)->next;

        // For rapid deallocation, this node may have been added to the free
        // list with some sub-trees still attached. We want to allocate an empty
        // node, and we don't want to leak any sub-trees attached to it, so go
        // through and deallocate any sub-trees belonging to this node.
        while ((*node)->sub_trees != 0) {
            size_t index = __builtin_ctzl((*node)->sub_trees);
            SymbolMapAllocator_Free(alloc, (*node)->entries[index].value);
            (*node)->sub_trees &= ~((uintptr_t)1 << index);
        }
    }
    assert((*node)->sub_trees == 0);

    for (size_t i = 0; i < SYMBOL_MAP_FANOUT; ++i) {
        (*node)->entries[i] = (SymbolNodeEntry) {
            .tag = SYMBOL_TAG_INVALID,
            .value = NULL,
        };
    }

    return BLIMP_OK;
}

void SymbolMapAllocator_Destroy(SymbolMapAllocator *alloc)
{
    while (alloc->free_nodes != NULL) {
        SymbolNode *node;
        CHECK(SymbolMapAllocator_Alloc(alloc, &node));
            // Get a node to free by calling the allocator, so that we go
            // through the allocation code path that strips any sub-trees from
            // the allocated node and adds them to the list of free nodes. If
            // we didn't do this we would leak any attached sub-trees when we
            // free the node.
        free(node);
    }
}

Status SymbolMap_Init(SymbolMap *map, SymbolMapAllocator *alloc)
{
    map->alloc = alloc;
    map->root = (SymbolNodeEntry) {
        .tag = SYMBOL_TAG_INVALID,
        .value = 0,
    };
    map->size = 0;
    return BLIMP_OK;
}

void SymbolMap_Destroy(SymbolMap *map)
{
    if (map->root.value != NULL && map->root.tag == SYMBOL_TAG_INVALID) {
        // If the root is in the "sub-tree" state, free the sub-tree.
        SymbolMapAllocator_Free(map->alloc, map->root.value);
    }
}

static inline uintptr_t SymbolTag(const Symbol *sym)
{
    assert(
        ((uintptr_t)sym & (((uintptr_t)1 << SYMBOL_MAP_TAG_SHIFT) - 1)) == 0);
    return (uintptr_t)sym >> SYMBOL_MAP_TAG_SHIFT;
}

Status SymbolMap_Emplace(
    SymbolMap *map,
    const Symbol *sym,
    SymbolMapEmplacement *empl,
    bool *created)
{
    empl->evicted_sibling = NULL;
    empl->added_to_parent = NULL;

    SymbolNode *parent = NULL;
    SymbolNodeEntry *curr = &map->root;
    uintptr_t tag = SymbolTag(sym);
    for (size_t i = 0; i <= SYMBOL_MAP_MAX_DEPTH; ++i) {
        assert(tag != SYMBOL_TAG_INVALID);

        // Optimistically handle the lookup cases first, since looking up an
        // existing symbol is more common than inserting a new symbol.
        //
        // First the case where the current entry uniquely matches the desired
        // symbol.
        if (curr->tag == tag) {
            empl->value = &curr->value;
            *created = false;
            return BLIMP_OK;
        }
        // Next, the case where the current entry is a sub-tree which
        // recursively contains a slot for the desired symbol.
        if (curr->tag == SYMBOL_TAG_INVALID) {
            if (curr->value == NULL) {
                // If we have not yet allocated a sub-tree for this tag, then
                // this is an insert after all. Since we are inserting into an
                // empty sub-tree, the current entry uniquely identifies `sym`,
                // and we can just stick `sym` into this entry directly, without
                // allocating a new sub-tree.
                curr->tag = tag;
                empl->value = &curr->value;

                // Add `curr` as a non-empty child of its parent.
                if (parent != NULL) {
                    // Save the old value of `parent->first` in case we have to
                    // revert this emplacement.
                    empl->added_to_parent = parent;
                    empl->old_parent_first = parent->first;

                    ptrdiff_t curr_index = curr - parent->entries;
                    assert(0 <= curr_index
                        && curr_index < (ptrdiff_t)SYMBOL_MAP_FANOUT);
                    curr->to_next = parent->first - curr_index;
                    parent->first = curr_index;
                }

                *created = true;
                return BLIMP_OK;
            } else {
                // There is already a sub-tree here, so this entry does not
                // uniquely identify any symbol. Just recurse into the sub-tree.
                uintptr_t index = tag & SYMBOL_MAP_INDEX_MASK;
                tag >>= SYMBOL_MAP_LOG_FANOUT;
                assert(index < SYMBOL_MAP_FANOUT);
                assert(tag != SYMBOL_TAG_INVALID);

                parent = (SymbolNode *)curr->value;
                curr = &parent->entries[index];
                continue;
            }
        }

        // The final case is where `entry->tag` is set, but is not equal to the
        // tag of interest, which indicates that the current entry uniquely
        // identifies some symbol, just not the one we want. We are inserting
        // `sym` into this sub-tree, so this entry will no longer uniquely
        // identify any symbol. We need to allocate a new sub-tree, move the
        // existing (symbol, value) pair to an entry in the new sub-tree, and
        // then recursively insert `sym` into the new sub-tree.
        SymbolNode *new_node;
        TRY(SymbolMapAllocator_Alloc(map->alloc, &new_node));
        if (empl->evicted_sibling == NULL) {
            // Note the location and value of the entry which is getting moved,
            // in case we have to abort this emplacement and move it back. Note
            // that we only do this if we haven't done it already. If we have
            // already moved `evicted_sibling` from a higher sub-tree in a
            // previous iteration of this loop, we want it to get moved all the
            // way back there when we abort.
            empl->evicted_sibling = curr;
            empl->sibling_tag = curr->tag;
            empl->sibling_value = curr->value;
        }
        // Move the existing symbol's entry down into the sub-tree.
        size_t new_entry_index = curr->tag & SYMBOL_MAP_INDEX_MASK;
        SymbolNodeEntry *new_entry = &new_node->entries[new_entry_index];
        new_entry->tag = curr->tag >> SYMBOL_MAP_LOG_FANOUT;
        new_entry->value = curr->value;
        // Add `new_entry` to `new_node`s list of children. It is the only
        // entry, so it's `to_next` field is 0 to indicate the end of the list.
        new_entry->to_next = 0;
        new_node->first = new_entry_index;
        // Switch `curr` from the "unique symbol" state to the "sub-tree" state.
        curr->tag = SYMBOL_TAG_INVALID;
        curr->value = (void *)new_node;
        // Set the bit in `parent`s bitmap to indicate that the sub-tree at
        // position `index` (that is, `curr`) is allocated.
        if (parent != NULL) {
            assert(parent->entries <= curr
                && curr < parent->entries + SYMBOL_MAP_FANOUT);
            parent->sub_trees |= ((uintptr_t)1 << (curr - parent->entries));
        }

        // Now we have reduced the situation to the case where we are trying to
        // emplace `sym` and we have discovered a non-empty sub-tree. We can
        // simply update `curr` and continue the loop.
        uintptr_t index = tag & SYMBOL_MAP_INDEX_MASK;
        tag >>= SYMBOL_MAP_LOG_FANOUT;
        assert(index < SYMBOL_MAP_FANOUT);
        assert(tag != SYMBOL_TAG_INVALID);
        parent = new_node;
        curr = &new_node->entries[index];
    }

    // We should not get here. Every symbol is guaranteed to have a unique word-
    // sized tag, but if we get here without returning from the loop, then we
    // have exhausted all of the bits in the symbol's tag without finding a
    // unique slot in the tree for it.
    assert(false);
    return ErrorMsg(
        map->alloc->blimp, BLIMP_ERROR, "unreachable in SymbolMap_Emplace");
}

void SymbolMap_AbortEmplace(SymbolMap *map, SymbolMapEmplacement *empl)
{
    if (empl->evicted_sibling != NULL) {
        SymbolMapAllocator_Free(map->alloc, empl->evicted_sibling->value);
        empl->evicted_sibling->tag = empl->sibling_tag;
        empl->evicted_sibling->value = empl->sibling_value;
    }
    if (empl->added_to_parent != NULL) {
        empl->added_to_parent->first = empl->old_parent_first;
    }
}

void **SymbolMap_Find(SymbolMap *map, const Symbol *sym)
{
    SymbolNodeEntry *curr = &map->root;
    uintptr_t tag = SymbolTag(sym);
    for (size_t i = 0; i <= SYMBOL_MAP_MAX_DEPTH; ++i) {
        assert(tag != SYMBOL_TAG_INVALID);

        // Optimistically handle the successful lookup case first, since the
        // common case for Find is to succeed.
        if (curr->tag == tag) {
            return &curr->value;
        }
        // If this entry doesn't uniquely identify `sym`, try recursing into a
        // sub-tree.
        if (curr->tag == SYMBOL_TAG_INVALID) {
            if (curr->value == NULL) {
                // If there is no sub-tree in the slot that should contain this
                // symbol, then the symbol is not in the map.
                return NULL;
            } else {
                // There is a sub-tree, which identifies at least 2 distinct
                // symbols, and is guaranteed to contain `sym` if `sym` is in
                // the map. Recurse into the sub-tree.
                uintptr_t index = tag & SYMBOL_MAP_INDEX_MASK;
                tag >>= SYMBOL_MAP_LOG_FANOUT;
                assert(index < SYMBOL_MAP_FANOUT);
                assert(tag != SYMBOL_TAG_INVALID);
                curr = &((SymbolNode *)curr->value)->entries[index];
                continue;
            }
        }

        return NULL;
    }

    // We should not get here. Every symbol is guaranteed to have a unique word-
    // sized tag, but if we get here without returning from the loop, then we
    // have exhausted all of the bits in the symbol's tag without finding a
    // unique slot in the tree for it.
    assert(false);
    return NULL;
}

// `it` must point to a valid entry. This function advances `it` so that it
// points to the first unique symbol entry in the sub-tree rooted at its
// original value. Note that this may be the original value of `it`, if it
// already points at a unique symbol entry.
static inline const SymbolNodeEntry *AdvanceToFirstLeaf(SymbolMapIterator *it)
{
    const SymbolNodeEntry *curr = it->path[it->depth];
    assert(curr->tag != SYMBOL_TAG_INVALID || curr->value != NULL);

    while (curr->tag == SYMBOL_TAG_INVALID) {
        assert(curr->value != NULL);
        assert(it->depth < SYMBOL_MAP_MAX_DEPTH);
        const SymbolNode *node = (const SymbolNode *)curr->value;
        curr = &node->entries[node->first];
        it->path[++it->depth] = curr;
    }

    return curr;
}

SymbolMapIterator SymbolMap_Iterator(const SymbolMap *map)
{
    SymbolMapIterator it = {
        .count = 0,
        .depth = 0,
    };
    it.path[0] = &map->root;

    if (map->size > 0) {
        // If there is a leaf to go to, go to it so that when SymbolMap_Next()
        // is called, the invariant that `it` always points to a leaf is upheld.
        AdvanceToFirstLeaf(&it);
    }

    return it;
}

bool SymbolMap_Next(
    const SymbolMap *map,
    SymbolMapIterator *it,
    const Symbol **sym,
    void **value)
{
    if (it->count >= map->size) {
        return false;
    }

    const SymbolNodeEntry *curr = it->path[it->depth];

    // If not exhausted, the iterator always points to a unique symbol entry,
    // which is the next one to yield.
    assert(curr->tag != SYMBOL_TAG_INVALID);
    *sym = curr->sym;
    *value = curr->value;

    if (++it->count == map->size) {
        // The iterator is exhausted now, and we will return `false` on the next
        // call to SymbolMap_Next(), but this time we need to return `true` for
        // the entry we yielded above.
        return true;
    }

    // Based on `count`, we haven't visited every entry yet, so there must be a
    // next unique symbol entry in the tree. Advance to it.
    do {
        if (curr->to_next) {
            // There is at least one more sibling in this sub-tree. Go to it.
            it->path[it->depth] = curr + curr->to_next;

            // If the sibling is a sub-tree, it must have at least one
            // symbol in it. Walk down to that symbol.
            curr = AdvanceToFirstLeaf(it);
            assert(curr->tag != SYMBOL_TAG_INVALID);
        } else {
            // `curr` was the last child in its sub-tree, so we start moving
            // b;ack up the tree.
            assert(it->depth > 0);
                // We cannot have visited all of `curr`s siblings if `curr` is
                // the root, because the root is an only child with no parent,
                // yet we know there are more unvisited symbols in the tree.
            curr = it->path[--it->depth];

            // If we're moving up into an entry, it must have a sub-tree below
            // it.
            assert(curr->tag == SYMBOL_TAG_INVALID);
            assert(curr->value != NULL);

            // Continue the loop, moving to the next entry in the new current
            // sub-tree (the old parent sub-tree).
        }
    } while (curr->tag == SYMBOL_TAG_INVALID);

    return true;
}
