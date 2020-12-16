#include <math.h>

#include "ordered_map.h"

#define ALIGNMENT (sizeof(size_t))

static inline size_t RoundUpToAlignment(size_t n)
{
    return (n + (ALIGNMENT - 1)) & ~(ALIGNMENT - 1);
}

typedef struct OrderedMapNode {
    struct OrderedMapNode *parent;
    struct OrderedMapNode *left;
    struct OrderedMapNode *right;
    enum {
        RED,
        BLACK,
    } color;
    bool pending;

    char key[] __attribute__((aligned (ALIGNMENT)));
    // Value is at offsetof(Node, key) + RoundUpToAlignment(key_size).
} Node;

// Because the OrderedMapNode structure is dynamically sized based on the
// `key_size` and `value_size` that the user gives us, we have to do our own
// size computation.
static inline size_t NodeSize(const OrderedMap *map)
{
    return sizeof(Node)
        + RoundUpToAlignment(map->key_size)
        + RoundUpToAlignment(map->value_size);
}

static inline char *NodeKey(const OrderedMap *map, Node *node)
{
    (void)map;
    return node->key;
}

static inline char *NodeValue(const OrderedMap *map, Node *node)
{
    return NodeKey(map, node) + RoundUpToAlignment(map->key_size);
}

static void RotateLeft(OrderedMap *map, Node *n1)
{
    // Transform the subtree rooted at `n1` so that its right child becomes the
    // new root, like so:
    //
    //          n1              n2                                            //
    //         /  \            /  \                                           //
    //        l   n2    =>    n1  r2                                          //
    //           /  \         / \                                             //
    //          r1  r2       l  r1                                            //
    //

    Node *n2 = n1->right;
    Node *r1 = n2->left;

    assert(n1 != NULL);
    assert(n2 != NULL);

    // Make r1 the right child of n1.
    n1->right = r1;
    if (r1 != NULL) {
        r1->parent = n1;
    }

    // Make n2 a child of n1's parent.
    n2->parent = n1->parent;
    if (n2->parent == NULL) {
        // n1's "parent" was the map itself.
        assert(map->root == n1);
        map->root = n2;
    } else if (n1 == n2->parent->left) {
        n2->parent->left = n2;
    } else {
        assert(n1 == n2->parent->right);
        n2->parent->right = n2;
    }

    // Make n1 the left child of n2.
    n2->left = n1;
    n1->parent = n2;
}

static void RotateRight(OrderedMap *map, Node *n1)
{
    // Transform the subtree rooted at `n1` so that its left child becomes the
    // new rot, like so:
    //
    //          n1              n2                                            //
    //         /  \            /  \                                           //
    //        n2   r    =>    l1  n1                                          //
    //       /  \                /  \                                         //
    //      l1  l2              l2   r                                        //
    //

    Node *n2 = n1->left;
    Node *l2 = n2->right;

    assert(n1 != NULL);
    assert(n2 != NULL);

    // Make l2 the left child of n1.
    n1->left = l2;
    if (l2 != NULL) {
        l2->parent = n1;
    }

    // Make n2 a child of n1's parent.
    n2->parent = n1->parent;
    if (n2->parent == NULL) {
        // n1's "parent" was the map itself.
        assert(map->root == n1);
        map->root = n2;
    } else if (n1 == n2->parent->left) {
        n2->parent->left = n2;
    } else {
        assert(n1 == n2->parent->right);
        n2->parent->right = n2;
    }

    // Make n1 the right child of n2.
    n2->right = n1;
    n1->parent = n2;
}
void OrderedMap_Init(
    Blimp *blimp,
    OrderedMap *map,
    size_t key_size,
    size_t value_size,
    CmpFunc cmp)
{
    map->blimp = blimp;
    map->size = 0;
    map->key_size = key_size;
    map->value_size = value_size;
    map->cmp = cmp;
    map->root = NULL;
}

void OrderedMap_Destroy(OrderedMap *map)
{
    Node *stack = NULL;

    // In order to free all of the nodes, we must traverse the tree. We will not
    // use an OrderedMapIterator to do this traversal, because the iterator
    // allocates memory to store a tempory stack of nodes representing the
    // traversal state. We don't want this iteration to fail in a low-memory
    // situation, since the whole point is to free up memory. Instead, we will
    // build our stack of nodes out of the tree itself, using each node's
    // `parent` pointer as the "next" pointer in a linked list. This corrupts
    // the structure of the tree, but that's alright because we're destroying it
    // anyways.

    // Traverse down to the left-most node, pushing nodes onto the stack as we
    // go so that we can continue the traversal upwards and rightwards after
    // visiting the leftmost node.
    for (Node *curr = map->root; curr != NULL; curr = curr->left) {
        curr->parent = stack;
        stack = curr;
    }

    while (stack != NULL) {
        // Pop the next node in the traversal from the stack.
        Node *curr = stack;
        stack = curr->parent;

        // The next node we should visit is the leftmost node in the subtree
        // rooted at `curr->right`. Traverse to that node and add everything in
        // between to the stack.
        for (Node *next = curr->right; next != NULL; next = next->left) {
            next->parent = stack;
            stack = next;
        }

        free(curr);
    }
}

void OrderedMap_Move(OrderedMap *from, OrderedMap *to)
{
    *to = *from;
    from->size = 0;
    from->root = NULL;
}

Status OrderedMap_Iterator(const OrderedMap *map, OrderedMapIterator *it)
{
    Vector_Init(map->blimp, it, sizeof(Node *), NULL);
    if (map->size > 0) {
        // The iterator will use a stack (implemented as a Vector) to keep track
        // of nodes that it still needs to visit. The stack always represents a
        // path from the root to a leaf. Since the theoretical maximum height of
        // a red-black tree is 2log(n), we can reserve enough memory now that we
        // will never fail to add a node to the stack during the traversal.
        TRY(Vector_Reserve(it, (size_t)ceil(2*log2(map->size))));

        // The first node we visit is the leftmost in the tree. Traverse to it,
        // and add everything we encounter along the way to the stack so we can
        // visit them when we have popped the first node off of the stack.
        for (Node *curr = map->root; curr != NULL; curr = curr->left) {
            CHECK(Vector_PushBack(it, &curr));
        }
    }
    return BLIMP_OK;
}

Status OrderedMap_RIterator(const OrderedMap *map, OrderedMapIterator *it)
{
    Vector_Init(map->blimp, it, sizeof(Node *), NULL);
    if (map->size > 0) {
        // The iterator will use a stack (implemented as a Vector) to keep track
        // of nodes that it still needs to visit. The stack always represents a
        // path from the root to a leaf. Since the theoretical maximum height of
        // a red-black tree is 2log(n), we can reserve enough memory now that we
        // will never fail to add a node to the stack during the traversal.
        TRY(Vector_Reserve(it, (size_t)ceil(2*log2(map->size))));

        // The first node we visit is the rightmost in the tree. Traverse to it,
        // and add everything we encounter along the way to the stack so we can
        // visit them when we have popped the first node off of the stack.
        for (Node *curr = map->root; curr != NULL; curr = curr->right) {
            CHECK(Vector_PushBack(it, &curr));
        }
    }
    return BLIMP_OK;
}

bool OrderedMap_Next(
    const OrderedMap *map,
    OrderedMapIterator *it,
    const void **key,
    void **value)
{
    if (Vector_Empty(it)) {
        // If the stack of nodes to visit is empty, we are done.
        Vector_Destroy(it);
        return false;
    }

    // Get the next node in the traversal from the stack.
    Node *curr;
    Vector_PopBack(it, &curr);

    if (key != NULL) {
        *key = NodeKey(map, curr);
    }
    if (value != NULL) {
        *value = NodeValue(map, curr);
    }

    // The next node we should visit is the leftmost node in the subtree rooted
    // at `curr->right`. Traverse to that node and add everything in between to
    // the stack.
    for (curr = curr->right; curr != NULL; curr = curr->left) {
        CHECK(Vector_PushBack(it, &curr));
    }

    return true;
}

bool OrderedMap_RNext(
    const OrderedMap *map,
    OrderedMapIterator *it,
    const void **key,
    void **value)
{
    if (Vector_Empty(it)) {
        // If the stack of nodes to visit is empty, we are done.
        Vector_Destroy(it);
        return false;
    }

    // Get the next node in the traversal from the stack.
    Node *curr;
    Vector_PopBack(it, &curr);

    if (key != NULL) {
        *key = NodeKey(map, curr);
    }
    if (value != NULL) {
        *value = NodeValue(map, curr);
    }

    // The next node we should visit is the right node in the subtree rooted
    // at `curr->left`. Traverse to that node and add everything in between to
    // the stack.
    for (curr = curr->left; curr != NULL; curr = curr->right) {
        CHECK(Vector_PushBack(it, &curr));
    }

    return true;
}

Status OrderedMap_Emplace(
    OrderedMap *map, const void *key, OrderedMapEntry **entry, bool *created)
{
    // First insert the node at a leaf without rebalancing. If this breaks any
    // red-black invariants, we will fix them as we walk back up the tree in a
    // second step.
    Node *parent = NULL;
        // The previous node in the traversal. `parent` will walk down the tree
        // until at least one of its children (the one where the new key should
        // be inserted) is NULL. At that point, `parent` will become the parent
        // of a new node containing `key`.
    Node **parents_child = &map->root;
        // A pointer to the field which points at the current node in the
        // traversal. The first node is the root, so we initialize
        // `parents_child` to point at the tree's root pointer. Later on, this
        // will point to either the `left` or `right` pointer of the parent of
        // the current node. This allows us to insert a new node without caring
        // whether it is the left or right child of its parent, or even whether
        // it is the root.

    int cmp;
    while (*parents_child != NULL) {
        parent = *parents_child;
        cmp = map->cmp(key, (*parents_child)->key);
        if (cmp == 0) {
            // If we find an existing node with a key exactly equal to `key`,
            // return without inserting a new node.
            *entry = *parents_child;
            if (created != NULL) {
                *created = false;
            }
            return BLIMP_OK;
        } else if (cmp < 0) {
            parents_child = &(*parents_child)->left;
        } else {
            parents_child = &(*parents_child)->right;
        }
    }

    // Insert a new red node (key, value) as a child of `parent`.
    TRY(Malloc(map->blimp, NodeSize(map), parents_child));
    Node *curr = *parents_child;
    curr->parent = parent;
    curr->left = NULL;
    curr->right = NULL;
    curr->color = RED;
    curr->pending = true;
    memcpy(NodeKey(map, curr), key, map->key_size);

    *entry = curr;
    if (created != NULL) {
        *created = true;
    }

    return BLIMP_OK;
}

void OrderedMap_CommitEmplace(OrderedMap *map, OrderedMapEntry *curr)
{
    if (!curr->pending) {
        return;
    }
    curr->pending = false;
    assert(curr->left == NULL);
    assert(curr->right == NULL);

    // Inserting the new node may have broken some of the red-black tree
    // invariants. For example, if `parent` is red, we now have two red nodes in
    // a row, which is not allowed. Walk back up the tree, fixing the red-black
    // invariants as we go.
    //
    // For reference, the red-black tree invariants are:
    //  1. The root is black.
    //  2. The parent of every red node is black (that is, there are no two
    //     consecutive red nodes, or "red bubbles").
    //  3. Every path from the root to a leaf contains the same number of black
    //     nodes.
    //
    // We will ignore invariant 1 for now, since it is a purely local property.
    // We will make sure to fix up the root and make it black at the end. In the
    // meantime, this loop will fix invariants 2 and 3.
    //
    // At the start of the loop, only invariant 2 can be broken, since adding a
    // new red node has not changed the number of black nodes on any path from
    // root to leaf. The loop will maintain invariant 3 and the loop invariant
    // that `curr` is red. As each iteration of the loop moves further up the
    // tree, we will ensure that invariant 2 holds for the section of the tree
    // below `curr` (the section that the loop has already fixed). We will walk
    // up the tree until `curr`s parent is black, at which point invariant 2
    // holds for `curr` and its parent, and it also holds for everything above,
    // since it was never broken higher up the tree.
    while (curr != map->root && curr->parent->color == RED) {
        assert(curr->color == RED);
        Node *parent      = curr->parent;
        Node *grandparent = parent->parent;
            // Note that `grandparent` is guaranteed to be non-NULL -- since
            // `parent` is red, it cannot be the root. Also, grandparent must be
            // black since `parent` is red.
        assert(grandparent != NULL);
        assert(grandparent->color == BLACK);

        if (parent == grandparent->left) {
            // Case 1: `parent` is a left child.
            //
            // grandparent-> b                                                //
            //              / \                                               //
            //    parent-> r   * <- uncle                                     //
            //            /                                                   //
            //    curr-> r                                                    //
            Node *uncle = grandparent->right;
            if (uncle && uncle->color == RED) {
                // Case 1a:
                //  * Swap the colors of `parent`, `uncle`, and `grandparent`.
                //  * Move `curr` to `grandparent`.
                //
                //         b          ->r                                     //
                //        / \          / \                                    //
                //       r   r   =>   b   b                                   //
                //      /            /                                        //
                //   ->r            r                                         //
                //
                // Since `parent` and `uncle` are both red, making both of them
                // black adds 1 black node to all paths which pass through
                // `grandparent` (since any such path passes through exactly one
                // of `parent` or `uncle`). Making `grandparent` red then
                // removes the extra black node from all such paths, preserving
                // invariant 3.
                //
                // In addition, making parent black fixes the red bubble between
                // `curr` and `parent`. We may create a new red bubble if
                // `grandparent`s parent is also red. But since we point `curr`
                // at `grandparent`, the invariant is allowed to be broken
                // there, and we will fix it on the next iteration of the loop.
                parent->color = BLACK;
                uncle->color = BLACK;
                grandparent->color = RED;
                curr = grandparent;
            } else {
                if (curr == parent->right) {
                    // Case 1b: for simplicity, left rotate about `parent` and
                    // swap `parent` and `curr`, so that this case reduces to
                    // case 1c.
                    //
                    //         b            b                                 //
                    //        / \          / \                                //
                    //       r   b   =>   r   b                               //
                    //        \          /                                    //
                    //       ->r      ->r                                     //
                    //
                    RotateLeft(map, parent);
                    curr = parent;
                    parent = curr->parent;
                }

                // Case 1c:
                //  * Swap the colors of `parent` and `grandparent`.
                //  * Right rotate about grandparent.
                //
                //         b              b                                   //
                //        / \            / \                                  //
                //       r   b   =>   ->r   r                                 //
                //      /                    \                                //
                //   ->r                      b                               //
                //
                // Swapping the colors moves the red bubble up from curr-parent
                // to parent-grandparent, and removes one black node from paths
                // passing through `uncle`, since `grandparent` is no longer
                // black. The rotation then breaks the red bubble by instead
                // creating a node with two red children (which is allowed), and
                // it ensures that any path through `uncle` also passes through
                // `parent` (which is black now) restoring invariant 3.
                //
                // The rotation also causes `curr` to move one step higher up
                // the tree, ensuring we make progress towards the root.
                parent->color = BLACK;
                grandparent->color = RED;
                RotateRight(map, grandparent);
            }
        } else {
            // Case 2: `parent` is a right child. This case is handled exactly
            // the same as case 1 above, except that all of the lefts and rights
            // are reversed.
            //
            // grandparent-> b                                                //
            //              / \                                               //
            //    uncle -> r   r <- parent                                    //
            //                  \                                             //
            //                   r <- curr                                    //
            Node *uncle = grandparent->left;
            if (uncle && uncle->color == RED) {
                // Case 2a:
                //
                //         b         -> r                                     //
                //        / \          / \                                    //
                //       r   r   =>   b   b                                   //
                //            \            \                                  //
                //          -> r            r                                 //
                //
                parent->color = BLACK;
                uncle->color = BLACK;
                grandparent->color = RED;
                curr = grandparent;
            } else {
                if (curr == parent->left) {
                    // Case 2b (rotate and fall through to case 2c):
                    //
                    //         b            r                                 //
                    //        / \          / \                                //
                    //       r   r   =>   b   r                               //
                    //          /              \                              //
                    //      -> r                r <-                          //
                    //
                    RotateRight(map, parent);
                    curr = parent;
                    parent = curr->parent;
                }

                // Case 2c:
                //         b               b                                  //
                //        / \             / \                                 //
                //       b   r     =>    r   r <-                             //
                //            \         /                                     //
                //             r <-    b                                      //
                parent->color = BLACK;
                grandparent->color = RED;
                RotateLeft(map, grandparent);
            }
        }
    }

    // Restore invariant 1 by making the root black. This is always safe because
    // all paths from the root to a leaf pass through the root, so we cannot be
    // changing the number of black nodes in any such path. And making a node
    // black cannot create a red bubble.
    map->root->color = BLACK;

    ++map->size;
}

void OrderedMap_AbortEmplace(OrderedMap *map, OrderedMapEntry *entry)
{
    if (!entry->pending) {
        return;
    }

    assert(entry->left == NULL);
    assert(entry->right == NULL);

    if (entry->parent == NULL) {
        assert(entry == map->root);
        assert(map->size == 0);
        map->root = NULL;
    } else if (entry == entry->parent->left) {
        entry->parent->left = NULL;
    } else {
        assert(entry == entry->parent->right);
        entry->parent->right = NULL;
    }

    free(entry);
}

void OrderedMap_GetEntry(
    const OrderedMap *map,
    OrderedMapEntry *entry,
    void **key,
    void **value)
{
    if (key != NULL) {
        *key = NodeKey(map, entry);
    }
    if (value != NULL) {
        *value = NodeValue(map, entry);
    }
}
