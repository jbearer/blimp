#ifndef BLIMP_SYMBOL_H
#define BLIMP_SYMBOL_H

#include "hash_map.h"

typedef HashMap SymbolTable;

PRIVATE Status SymbolTable_Init(Blimp *blimp, SymbolTable *symbols);
PRIVATE Status SymbolTable_GetSymbol(
    SymbolTable *symbols,
    const char *name,
    size_t length,
    const Symbol **symbol);
PRIVATE void SymbolTable_Destroy(SymbolTable *symbols);

PRIVATE bool SymbolEq(const Symbol **sym1, const Symbol **sym2, void *arg);
PRIVATE size_t SymbolHash(const Symbol **symbol, void *arg);

////////////////////////////////////////////////////////////////////////////////
// SymbolMap
//
// A SymbolMap is an efficient container representing a mapping from symbols to
// pointer-sized values.
//
// The implementation takes advantage of that fact that symbols are
// deduplicated, and thus a symbol can be represented uniquely by its address.
// Thus, a set of symbols is essentially a sparse set of integers (addresses)
// which can be represented as a radix bit trie.
//
// The bits in the address of a symbol are divided into fixed-size chunks, each
// of which can be interpeted as a small integer. These chunks are used as
// indices into an array of sub-trees at each node, and thus an address, or a
// sequence of chunks, represents a path down the tree.
//
// However, it would be inefficient to store each symbol at the end of its path.
// Such paths have length log_r(n), where `r` is the radix of the tree and `n`
// is the number of symbols in a complete tree, 2^(64 - a), where `a` is the
// number of reserved 0 bits in a symbol address due to alignment constraints.
// On most systems `a` is 3, so `n` is 2^61, and a reasonable choice of `r` is
// 32, so traversing such a path requires around 12 dereferences. At that piont,
// a hash table is likely more efficient.
//
// Instead, we adopt the typical radix tree optimizations of merging only-
// children with their parent nodes. That is, if there would only be one symbol
// in a particular sub-tree, that sub-tree is not represented as a sub-tree at
// all, but rather as a pointer to the value associated with the unique symbol
// and a tag identifying the symbol. Thus, we only need to traverse additional
// steps in a path as long as there are collisions in the prefixes of the symbol
// we are looking up and another symbol in the map. The birthday paradox is
// working against us here, but the sparsity of the map works in our favor, and
// the probability of having paths more than 2 or 3 nodes in length is
// exceedingly small, with paths of length 1 being quite common.
//

#define SYMBOL_MAP_LOG_FANOUT 5
    // The choice of fanout (or radix) is designed to strike a balance between
    // performance and memory usage. A higher fanout decreases the likelihood of
    // tag collisions, and thus shortens the average path to a unique symbol in
    // the tree, but each additional fanout requires 2 extra words of storage
    // per node (for an additional entry consisting of a tag and value), so each
    // increment to SYMBOL_MAP_LOG_FANOUT effectively doubles the size of a
    // node.
    //
    // The value of 5, for 32 entries per node and a node size of
    // 512 + sizeof(SymbolNode) (assuming a word size of 8 bytes) is deemed
    // reasonable.
#define SYMBOL_MAP_MAX_DEPTH (sizeof(uintptr_t)*CHAR_BIT/SYMBOL_MAP_LOG_FANOUT)

// SymbolNodeEntry: an entry in a node, representing a child.
//
// A node entry is either null, a pointer to a child node, or a
// `const Symbol *` if the child would only have one unique symbol in it.
//  * null: `tag == SYMBOL_TAG_INVALID`, `value == NULL`
//  * child: `tag == SYMBOL_TAG_INVALID`, value: SymbolNode*
//  * symbol: `tag != SYMBOL_TAG_INVALID`, tag: const Symbol*, value: the value
//      associated with this symbol in the map
typedef struct {
    uintptr_t tag;
        // If valid, `tag` represents the remaining bits in the address not yet
        // used to traverse to this node. In other words, `tag` is the
        // concatenation of the remaining chunks in the path representing this
        // symbol. It uniquely represents the symbol at this entry, because any
        // two symbols which reach this entry must share a prefix and thus have
        // different suffixes (tags).
    void *value;
        // The value associated with this symbol (if
        // `tag != SYMBOL_TAG_INVALID`) or a pointer to a sub-tree.
    int8_t to_next;
        // The offset from this entry to the next entry (in reverse insertion
        // order) in it's parent node. This is used to quickly find the next
        // child of the parent sub-tree when iterating.
        //
        // We use an offset rather than an absolute index so that we can
        // calculate the next entry from merely a pointer to this entry, without
        // knowing this entry's index or the base pointer stored in the parent
        // node.
    const Symbol *sym;
} SymbolNodeEntry;

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
    uintptr_t sibling_tag;
        // If `evicted_sibling` is set, `sibling_tag` is the tag representing
        // the unique symbol it originally held.
    void *sibling_value;
        // If `evicted_sibling` is set, `sibling_value` is the value associated
        // with the evicted symbol in the map.
    SymbolNode *added_to_parent;
        // If the emplacement caused a new unique symbol entry to be added to a
        // parent node, the parent node is stored here so we can reset the
        // parent's `first` index if we abort the emplacement.
    uint32_t old_parent_first;
        // The old value of `added_to_parent->first`, if applicable.
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
    SymbolMap *map,
    const Symbol *sym,
    SymbolMapEmplacement *empl,
    bool *created);

static inline void **SymbolMap_CommitEmplace(
    SymbolMap *map, SymbolMapEmplacement *empl)
{
    ++map->size;
    return empl->value;
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
