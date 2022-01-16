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
// children with there parent nodes. That is, if there would only be one symbol
// in a particular sub-tree, that sub-tree is not represented as a sub-tree at
// all, but rather as a pointer to the value associated with the unique symbol
// and a tag identifying the symbol. Thus, we only need to traverse additional
// steps in a path as long as there are collisions in the prefixes of the symbol
// we are looking up and another symbol in the map. The birthday paradox is
// working against us here, but the sparsity of the map works in our favor, and
// the probability of having paths more than 2 or 3 nodes in length is
// exceedingly small, with paths of length 1 being quite common.
//

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
} SymbolNodeEntry;

typedef struct SymbolNode {
    uint32_t sub_trees;
        // Bitmap of allocated child trees. There is a subtree at index `i` if
        // and only if the `i`th lowest order bit is set. This is used to
        // quickly find and deallocate all of the sub-trees of this node.
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
} SymbolMapEmplacement;

// An iterator over a symbol map is just the complete tag of a symbol.
// Conceptually, we treat the map like a set of integers and increment the
// iterator through all possible integers, from 0 to UINTPTR_MAX. In practice,
// the iterator is made efficient by skipping entire empty sub-trees in a single
// operation, by incrementing the chunk represeting the index of that sub-tree,
// which may not be the lowest-order chunk of bits in the iterator.
typedef uintptr_t SymbolMapIterator;

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

static inline SymbolMapIterator SymbolMap_Iterator(const SymbolMap *map)
{
    (void)map;
    return 0;
}

PRIVATE bool SymbolMap_Next(
    const SymbolMap *map,
    SymbolMapIterator *it,
    const Symbol **sym,
    void **value);

#endif // BLIMP_SYMBOL_H
