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

Status SymbolTable_Init(Blimp *blimp, SymbolTable *t)
{
    TRY(HashMap_Init(
        blimp, &t->index, sizeof(String), sizeof(Symbol *),
        (EqFunc)StringEq, (HashFunc)StringHash, NULL));
    Vector_Init(blimp, &t->symbols, sizeof(Symbol *), FreeDestructor);
    return BLIMP_OK;
}

void SymbolTable_Destroy(SymbolTable *t)
{
    HashMap_Destroy(&t->index);
    Vector_Destroy(&t->symbols);
}

Status SymbolTable_GetSymbol(
    SymbolTable *t,
    const char *name,
    size_t length,
    const Symbol **symbol)
{
    Blimp *blimp = Vector_GetBlimp(&t->symbols);

    // Check if a symbol with this name already exists in the index.
    HashMapEntry *entry;
    bool created;
    TRY(HashMap_Emplace(&t->index, &(String){name, length}, &entry, &created));

    String *key;
    Symbol **value;
    size_t hash;
    HashMap_GetEntry(&t->index, entry, (void **)&key, (void **)&value, &hash);

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
        HashMap_AbortEmplace(&t->index, entry);
        return ret;
    }
    // The value of the entry is completely uninitialized. We need to create a
    // new symbol for it:
    Symbol *new_symbol;
    if ((ret = Malloc(blimp, sizeof(Symbol), &new_symbol)) != BLIMP_OK) {
        HashMap_AbortEmplace(&t->index, entry);
        return ret;
    }
    Object_Init((Object *)new_symbol, blimp, OBJ_SYMBOL);
    new_symbol->length = length;
    new_symbol->name   = key->data;
    new_symbol->hash   = hash;
    new_symbol->index  = Vector_Length(&t->symbols);
    *value  = new_symbol;
    *symbol = new_symbol;

    // Add the new symbol to the list of all symbols.
    if ((ret = Vector_PushBack(&t->symbols, &new_symbol)) != BLIMP_OK) {
        HashMap_AbortEmplace(&t->index, entry);
        Free(blimp, &new_symbol);
        return ret;
    }

    HashMap_CommitEmplace(&t->index, entry);
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

#define SYMBOL_MAP_FANOUT ((uintptr_t)1 << SYMBOL_MAP_LOG_FANOUT)
#define SYMBOL_MAP_INDEX_MASK (SYMBOL_MAP_FANOUT - 1)
    // A bitmask for the next chunk of bits in a packed path.
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

#define ENTRY_TYPE_EMPTY ((SymbolIndex)0)
#define ENTRY_TYPE_SUB_TREE ((SymbolIndex)1)
#define ENTRY_TYPE_SYMBOL ((SymbolIndex)2)

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
        *node = (SymbolNode *)PoolAllocator_Alloc(&alloc->base);
        if (*node == NULL) {
            return Error(alloc->blimp, BLIMP_OUT_OF_MEMORY);
        }
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
            assert((*node)->entries[index].type == ENTRY_TYPE_SUB_TREE);
            SymbolMapAllocator_Free(alloc, (*node)->entries[index].value);
            (*node)->sub_trees &= ~((uintptr_t)1 << index);
        }
    }
    assert((*node)->sub_trees == 0);

    // Make sure all of the entry types get set to ENTRY_TYPE_EMPTY.
    assert(ENTRY_TYPE_EMPTY == 0);
    memset((*node)->entries, 0, SYMBOL_MAP_FANOUT*sizeof(SymbolNodeEntry));

    return BLIMP_OK;
}

void SymbolMapAllocator_Init(Blimp *blimp, SymbolMapAllocator *alloc)
{
    alloc->blimp = blimp;
    alloc->free_nodes = NULL;
    PoolAllocator_Init(
        &alloc->base,
        sizeof(SymbolNode) + SYMBOL_MAP_FANOUT*sizeof(SymbolNodeEntry),
        blimp->options.gc_batch_size,
        // No GC or one-time initializer.
        0, NULL, NULL, NULL);
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
        PoolAllocator_Free(&alloc->base, node);
    }

    PoolAllocator_Destroy(&alloc->base);
}

Status SymbolMap_Init(SymbolMap *map, SymbolMapAllocator *alloc)
{
    map->alloc = alloc;
    map->root = (SymbolNodeEntry) {
        .type = ENTRY_TYPE_EMPTY,
    };
    map->size = 0;
    return BLIMP_OK;
}

void SymbolMap_Destroy(SymbolMap *map)
{
    if (map->root.type == ENTRY_TYPE_SUB_TREE) {
        // If the root is in the "sub-tree" state, free the sub-tree.
        SymbolMapAllocator_Free(map->alloc, map->root.value);
    }
}

Status SymbolMap_Emplace(
    SymbolMap *map, const Symbol *sym, SymbolMapEmplacement *empl)
{
    empl->evicted_sibling = NULL;
    empl->added_to_parent = NULL;

    SymbolNode *parent = NULL;
    SymbolNodeEntry *curr = &map->root;
    SymbolIndex path = sym->index;
    for (size_t i = 0; i <= SYMBOL_MAP_MAX_DEPTH; ++i) {
        switch (curr->type) {
            case ENTRY_TYPE_SYMBOL:
                // If the current entry is a unique symbol, we have two cases:
                if (curr->index == sym->index) {
                    // Case 1: it is the symbol we are emplacing. In this case,
                    // we just get a pointer to the value. This is an update,
                    // not an insert.
                    empl->entry = curr;
                    empl->old_value = curr->value;
                    empl->created = false;
                    return BLIMP_OK;
                } else {
                    // Case 2: it is a different symbol. We are inserting `sym`
                    // into this sub-tree, so this entry will no longer uniquely
                    // identify any symbol. We need to allocate a new sub-tree,
                    // move the existing (symbol, value) pair to an entry in the
                    // new sub-tree, and then recursively insert `sym` into the
                    // new sub-tree.
                    //
                    // `sym` and the current symbol are likely to have indices
                    // that differ in the next chunk of bits, so they will go in
                    // different slots in the new sub-tree, but even if they
                    // don't, and they collide once again, the next iteration of
                    // this loop will hit this same case and move the original
                    // symbol even further down the tree. Collisions become
                    // decreasingly likely with each iteration, and eventually
                    // impossible once we have accounted for all of the bits in
                    // the symbol indices.
                    SymbolNode *new_node;
                    TRY(SymbolMapAllocator_Alloc(map->alloc, &new_node));
                    if (!empl->evicted_sibling) {
                        // Note the location of the entry which is getting
                        // moved, in case we have to abort this emplacement and
                        // move it back.
                        //
                        // Note that we only do this if we haven't done it
                        // already. If we have already moved `evicted_sibling`
                        // from a higher sub-tree in a previous iteration of
                        // this loop, we want it to get moved all the way back
                        // there when we abort.
                        empl->evicted_sibling = curr;
                        empl->evicted_from = parent;
                    }
                    // Move the existing symbol's entry down into the sub-tree.
                    size_t new_entry_index =
                        (curr->index >> (i*SYMBOL_MAP_LOG_FANOUT))
                      & SYMBOL_MAP_INDEX_MASK;
                    SymbolNodeEntry *new_entry =
                        &new_node->entries[new_entry_index];
                    new_entry->type = ENTRY_TYPE_SYMBOL;
                    new_entry->index = curr->index;
                    new_entry->value = curr->value;
                    // Add `new_entry` to `new_node`s list of children. It is
                    // the only entry, so it's `to_next` field is 0 to indicate
                    // the end of the list.
                    new_entry->to_next = 0;
                    new_node->first = new_entry_index;
                    // Switch `curr` from the "unique symbol" state to the
                    // "sub-tree" state.
                    curr->type = ENTRY_TYPE_SUB_TREE;
                    curr->value = (void *)new_node;
                    // Set the bit in `parent`s bitmap to indicate that the
                    // sub-tree at position `index` (that is, `curr`) is
                    // allocated.
                    if (parent != NULL) {
                        assert(parent->entries <= curr
                            && curr < parent->entries + SYMBOL_MAP_FANOUT);
                        parent->sub_trees |=
                            ((uintptr_t)1 << (curr - parent->entries));
                    }

                    // Now we have reduced the situation to the case where we
                    // are trying to emplace `sym` and we have discovered a
                    // non-empty sub-tree. We can simply update `curr` and
                    // continue the loop.
                    uintptr_t index = path & SYMBOL_MAP_INDEX_MASK;
                    path >>= SYMBOL_MAP_LOG_FANOUT;
                    assert(index < SYMBOL_MAP_FANOUT);
                    parent = new_node;
                    curr = &new_node->entries[index];
                    continue;
                }
            case ENTRY_TYPE_SUB_TREE: {
                // If the current entry is a sub-tree, recurse into it.
                uintptr_t index = path & SYMBOL_MAP_INDEX_MASK;
                path >>= SYMBOL_MAP_LOG_FANOUT;
                assert(index < SYMBOL_MAP_FANOUT);
                parent = (SymbolNode *)curr->value;
                curr = &parent->entries[index];
                continue;
            }
            case ENTRY_TYPE_EMPTY:
                // If we have not yet allocated a sub-tree for this index, then
                // this is definitely an insert. Since we are inserting into an
                // empty sub-tree, the current entry uniquely identifies `sym`,
                // and we can just stick `sym` into this entry directly, without
                // allocating a new sub-tree.
                curr->type = ENTRY_TYPE_SYMBOL;
                curr->index = sym->index;
                empl->entry = curr;
                empl->old_value = NULL;
                    // This is an insert, there was no old value.

                if (parent != NULL) {
                    // Save the parent of the newly inserted entry in case we
                    // have to revert this emplacement and remove the new entry
                    // from `parent`s list of non-empty entries.
                    empl->added_to_parent = parent;

                    // Add `curr` as a non-empty child of its parent.
                    ptrdiff_t curr_index = curr - parent->entries;
                    assert(0 <= curr_index
                        && curr_index < (ptrdiff_t)SYMBOL_MAP_FANOUT);
                    curr->to_next = parent->first - curr_index;
                    parent->first = curr_index;
                }

                empl->created = true;
                return BLIMP_OK;
        }
    }

    // We should not get here. Every symbol is guaranteed to have a unique
    // index, but if we get here without returning from the loop, then we
    // have exhausted all of the bits in the symbol's index without finding a
    // unique slot in the tree for it.
    assert(false);
    return ErrorMsg(
        map->alloc->blimp, BLIMP_ERROR, "unreachable in SymbolMap_Emplace");
}

static void FlattenSingletonSubtree(
    SymbolMap *map, SymbolNode *parent, SymbolNodeEntry *entry)
{
    assert(entry->type != ENTRY_TYPE_EMPTY);
        // An empty sub-tree is not a singleton sub-tree.
    if (entry->type == ENTRY_TYPE_SYMBOL) {
        // The sub-tree is already flat.
        return;
    }
    assert(entry->type == ENTRY_TYPE_SUB_TREE);

    SymbolNode *sub_tree = entry->value;
    SymbolNodeEntry *sub_entry = &sub_tree->entries[sub_tree->first];
    assert(sub_entry->type != ENTRY_TYPE_EMPTY);
        // `first` should never point to an empty entry.
    assert(sub_entry->to_next == 0);
        // The sub-tree must be a singleton.
    FlattenSingletonSubtree(map, sub_tree, sub_entry);

    // Move the `sub_entry`, which now represents the unique symbol in
    // `sub_tree`, into `entry`, so we can delete `sub_tree`.
    assert(sub_entry->type == ENTRY_TYPE_SYMBOL);
    entry->type = ENTRY_TYPE_SYMBOL;
    entry->index = sub_entry->index;
    entry->value = sub_entry->value;
    assert(sub_tree->sub_trees == 0);
        // The recursive call should have cleared the bit representing the
        // sub-tree we just flattened, which should have been the last sub-tree
        // under `sub_tree`.
    SymbolMapAllocator_Free(map->alloc, sub_tree);

    if (parent != NULL) {
        // Clear the sub-tree bit for this entry in `parent`.
        parent->sub_trees &= ~((uintptr_t)1 << parent->first);
    }
}

void SymbolMap_AbortEmplace(SymbolMap *map, SymbolMapEmplacement *empl)
{
    empl->entry->value = empl->old_value;
    if (empl->created) {
        // If we created a new entry, we must remove it when we abort.
        assert(empl->entry->type == ENTRY_TYPE_SYMBOL);
        empl->entry->type = ENTRY_TYPE_EMPTY;

        // If there was a parent node, we must have added the new entry to its
        // non-empty child list. We need to remove it now.
        if (empl->added_to_parent != NULL) {
            // Compute the index of the old first entry, by finding the absolute
            // index of the added entry, then using the offset to its successor
            // to find the absolute index of the successor entry.
            ptrdiff_t entry_index = empl->entry - empl->added_to_parent->entries;
            assert(0 <= entry_index && (size_t)entry_index < SYMBOL_MAP_FANOUT);
            ptrdiff_t succ_index = entry_index + empl->entry->to_next;
            assert(0 <= succ_index && (size_t)succ_index < SYMBOL_MAP_FANOUT);
            empl->added_to_parent->first = succ_index;
        }
    } else {
        assert(empl->added_to_parent == NULL);
            // We cannot have added a new entry to the parent node's non-empty
            // list if we didn't create a new entry.
    }

    if (empl->evicted_sibling != NULL) {
        FlattenSingletonSubtree(map, empl->evicted_from, empl->evicted_sibling);
    }
}

void **SymbolMap_Find(SymbolMap *map, const Symbol *sym)
{
    SymbolNodeEntry *curr = &map->root;
    SymbolIndex path = sym->index;
    for (size_t i = 0; i <= SYMBOL_MAP_MAX_DEPTH; ++i) {
        switch (curr->type) {
            case ENTRY_TYPE_SYMBOL:
                // If this entry identifies a unique symbol, then it is either
                // our symbol, or our symbol is not in the tree.
                if (curr->index == sym->index) {
                    return &curr->value;
                } else {
                    return NULL;
                }
            case ENTRY_TYPE_SUB_TREE: {
                // If this entry is a sub-tree which identifies at least 2
                // distinct symbols, it is guaranteed to contain `sym` if `sym`
                // is in the map. Recurse into the sub-tree.
                size_t index = path & SYMBOL_MAP_INDEX_MASK;
                path >>= SYMBOL_MAP_LOG_FANOUT;
                assert(index < SYMBOL_MAP_FANOUT);
                curr = &((SymbolNode *)curr->value)->entries[index];
                continue;
            }
            case ENTRY_TYPE_EMPTY:
                // If this entry contains no symbols, then `sym` is not in the
                // map.
                return NULL;
        }
    }

    // We should not get here. Every symbol is guaranteed to have a unique
    // index, but if we get here without returning from the loop, then we
    // have exhausted all of the bits in the symbol's index without finding a
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
    assert(curr->type != ENTRY_TYPE_EMPTY);

    while (curr->type == ENTRY_TYPE_SUB_TREE) {
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
    assert(curr->type == ENTRY_TYPE_SYMBOL);
    *sym = SymbolTable_Index(&map->alloc->blimp->symbols, curr->index);
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
            assert(curr->type == ENTRY_TYPE_SYMBOL);
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
            assert(curr->type == ENTRY_TYPE_SUB_TREE);

            // Continue the loop, moving to the next entry in the new current
            // sub-tree (the old parent sub-tree).
        }
    } while (curr->type == ENTRY_TYPE_SUB_TREE);

    return true;
}
