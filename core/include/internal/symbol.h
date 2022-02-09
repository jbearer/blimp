#ifndef BLIMP_SYMBOL_H
#define BLIMP_SYMBOL_H

#include "hash_map.h"
#include "vector.h"

typedef uint32_t SymbolIndex;

typedef struct {
    Vector/*<const Symbol *>*/ symbols;
        // All unique symbols. Each symbol `s` is at index `s->index`.
    HashMap/*<String, const Symbol *>*/ index;
        // Symbols indexed by name.
} SymbolTable;

PRIVATE Status SymbolTable_Init(Blimp *blimp, SymbolTable *symbols);
PRIVATE Status SymbolTable_GetSymbol(
    SymbolTable *symbols,
    const char *name,
    size_t length,
    const Symbol **symbol);
PRIVATE void SymbolTable_Destroy(SymbolTable *symbols);

static inline const Symbol *SymbolTable_Index(SymbolTable *t, SymbolIndex index)
{
    return *(const Symbol **)Vector_Index(&t->symbols, index);
}

PRIVATE bool SymbolEq(const Symbol **sym1, const Symbol **sym2, void *arg);
PRIVATE size_t SymbolHash(const Symbol **symbol, void *arg);

////////////////////////////////////////////////////////////////////////////////
// SymbolMap
//
// A SymbolMap is an efficient container representing a mapping from symbols to
// pointer-sized values.
//
// The implementation takes advantage of that fact that symbols are
// deduplicated, and thus a symbol can be represented uniquely by its index into
// the symbol table. Thus, a set of symbols is essentially a sparse set of
// integers (indices) which can be represented as a radix bit trie.
//
// The bits in the index of a symbol are divided into fixed-size chunks, each
// of which can be interpeted as a small integer. These chunks are used as
// indices into an array of sub-trees at each node, and thus an index, or a
// sequence of chunks, represents a path down the tree.
//
// However, it would be inefficient to store each symbol at the end of its path.
// Such paths have length log_r(n), where `r` is the radix of the tree and `n`
// is the number of symbols in a complete tree, 2^32. A reasonable choice of `r`
// is 32, so traversing such a path requires around 12 dereferences. At that
// piont, a hash table is likely more efficient.
//
// Instead, we adopt the typical radix tree optimization of merging only-
// children with their parent nodes. That is, if there would only be one symbol
// in a particular sub-tree, that sub-tree is not represented as a sub-tree at
// all, but rather as a pointer to the value associated with the unique symbol
// and the index identifying the symbol. Thus, we only need to traverse
// additional steps in a path as long as there are collisions in the prefixes of
// the symbol we are looking up and another symbol in the map. The birthday
// paradox is working against us here, but the sparsity of the map works in our
// favor, and the probability of having paths more than 2 or 3 nodes in length
// is exceedingly small, with paths of length 1 being quite common.
//

#define SYMBOL_MAP_LOG_FANOUT 5
    // The choice of fanout (or radix) is designed to strike a balance between
    // performance and memory usage. A higher fanout decreases the likelihood of
    // index chunk collisions, and thus shortens the average path to a unique
    // symbol in the tree, but each additional fanout requires 2 extra words of
    // storage per node (for an additional SymbolNodeEntry), so each increment
    // to SYMBOL_MAP_LOG_FANOUT effectively doubles the size of a node.
    //
    // The value of 5, for 32 entries per node and a node size of
    // 512 + sizeof(SymbolNode) (assuming a word size of 8 bytes) is deemed
    // reasonable.
#define SYMBOL_MAP_MAX_DEPTH (sizeof(uintptr_t)*CHAR_BIT/SYMBOL_MAP_LOG_FANOUT)

typedef uint8_t SymbolNodeEntryType;
    // There are 3 types of entries in a tree:
    //  * ENTRY_TYPE_SYMBOL: an entry representing a unique symbol
    //  * ENTRY_TYPE_SUB_TREE: a pointer to a sub-tree containing more than one
    //      symbol
    //  * ENTRY_TYPE_EMPTY: an unoccupied slot in the tree

// An entry in a node, representing a slod for a symbol or sub-tree.
typedef struct {
    void *value;
        // The value associated with this symbol (if
        // `type == ENTRY_TYPE_SYMBOL`) or a pointer to a sub-tree (if
        // `type == ENTRY_TYPE_SUB_TREE`).
    SymbolIndex index;
        // If `type == ENTRY_TYPE_SYMBOL`, the index into the global symbol
        // table of the symbol stored at this entry. `index` uniquely identifies
        // the symbol stored here. It can also be used to retrieve the address
        // of the symbol (by indexing into the symbol table) during iteration.
    int8_t to_next;
        // The offset from this entry to the next entry (in reverse insertion
        // order) in it's parent node. This is used to quickly find the next
        // child of the parent sub-tree when iterating.
        //
        // We use an offset rather than an absolute index so that we can
        // calculate the next entry from merely a pointer to this entry, without
        // knowing this entry's index or the base pointer stored in the parent
        // node.
    SymbolNodeEntryType type;
        // The type of this entry (unique symbol, sub-tree, or empty slot).
} SymbolNodeEntry;

_Static_assert(sizeof(SymbolNodeEntry) <= 2*sizeof(void *),
    "SymbolNodeEntry is using more than 2 words of memory..."
    "that's too much, man!");

typedef struct SymbolNode {
    uint32_t sub_trees;
        // Bitmap of allocated child trees. There is a subtree at index `i` if
        // and only if the `i`th lowest order bit is set. This is used to
        // quickly find and deallocate all of the sub-trees of this node.
    uint32_t first;
        // The index of the first child (in reverse insertion order) of this
        // node. The child can be a sub-tree or a leaf. This is used to quickly
        // find the next child of a sub-tree when iterating.
    struct SymbolNode *next;
        // When a node is in a free list, this is the next free node.
    SymbolNodeEntry entries[];
} SymbolNode;

// Allocator for symbol map nodes. Since all nodes are the same size, we use a
// simple stack-based allocator where allocation simply requires removing the
// first node from the free list. However, to speed up deallocation (since there
// is no guarantee that a deallocated node will be reused, and making it ready
// to reuse would require some work) the deallocator leaves any sub-trees of the
// node being deallocated attached to that node, rather than adding to the free
// list. Only when a node is allocated are its leftover sub-trees removed and
// added to the free list.
typedef struct {
    Blimp *blimp;
    SymbolNode *free_nodes;
} SymbolMapAllocator;

typedef struct {
    SymbolMapAllocator *alloc;
    SymbolNodeEntry root;
    size_t size;
} SymbolMap;

// SymbolMap implements an emplacement API, similar to HashMap.
// SymbolMapEmplacement contains data required to undo an emplace operation in
// the case of SymbolMap_AbortEmplace().
typedef struct {
    void **value;
        // A pointer to the emplaced value.
    SymbolNodeEntry *evicted_sibling;
        // If the emplacement caused a unique symbol to be replaced by a
        // sub-tree containing two symbols, we store the entry that originally
        // contained the unique symbol, so we can restore it to a unique symbol
        // in the case of an abort.
    SymbolNodeEntry *new_sibling;
        // If `evicted_sibling` is set, `new_sibling` is the node the evicted
        // sibling was moved to, so that we can restore its contents to
        // `evicted_sibling` in the case of an abort.
    SymbolNode *added_to_parent;
        // If the emplacement caused a new unique symbol entry to be added to a
        // parent node, the parent node is stored here so we can reset the
        // parent's `first` index if we abort the emplacement.
    uint32_t old_parent_first;
        // The old value of `added_to_parent->first`, if applicable.
    bool created;
        // Whether a new entry was created (`true`) or an old one updated
        // (`false`).
} SymbolMapEmplacement;

// An iterator over a symbol map is a path down the tree. When we encounter an
// empty sub-tree, or when we visit the last symbol in a sub-tree, we can easily
// skip the entire sub-tree by incrementing the corresponding index in `path`
// and discarding the corresponding suffix of `path`.
typedef struct {
    const SymbolNodeEntry *path[SYMBOL_MAP_MAX_DEPTH + 1];
        // The nodes we have traversed to reach the current position in the
        // tree. `path[0]` is always the root.
    size_t depth;
        // The depth in the tree of the current position in the traversal. Since
        // `path[0]` is always the root, `path[depth]` is always the last valid
        // entry in `path`.
    size_t count;
        // The number of entries we have visited. This is used to short-circuit
        // the traversal once we have visited all the entries in the map.
} SymbolMapIterator;

static inline void SymbolMapAllocator_Init(
    Blimp *blimp, SymbolMapAllocator *alloc)
{
    alloc->blimp = blimp;
    alloc->free_nodes = NULL;
}

PRIVATE void SymbolMapAllocator_Destroy(SymbolMapAllocator *alloc);
PRIVATE Status SymbolMap_Init(SymbolMap *map, SymbolMapAllocator *alloc);
PRIVATE void SymbolMap_Destroy(SymbolMap *map);
PRIVATE Status SymbolMap_Emplace(
    SymbolMap *map, const Symbol *sym, SymbolMapEmplacement *empl);

static inline void **SymbolMap_CommitEmplace(
    SymbolMap *map, SymbolMapEmplacement *empl)
{
    if (empl->created) {
        ++map->size;
    }
    return empl->value;
}

static inline Status SymbolMap_Update(
    SymbolMap *map, const Symbol *sym, void *value)
{
    SymbolMapEmplacement empl;
    TRY(SymbolMap_Emplace(map, sym, &empl));
    *SymbolMap_CommitEmplace(map, &empl) = value;
    return BLIMP_OK;
}

PRIVATE void SymbolMap_AbortEmplace(SymbolMap *map, SymbolMapEmplacement *empl);
PRIVATE void **SymbolMap_Find(SymbolMap *map, const Symbol *sym);

PRIVATE SymbolMapIterator SymbolMap_Iterator(const SymbolMap *map);
PRIVATE bool SymbolMap_Next(
    const SymbolMap *map,
    SymbolMapIterator *it,
    const Symbol **sym,
    void **value);

#endif // BLIMP_SYMBOL_H
