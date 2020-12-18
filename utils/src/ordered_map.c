#include <math.h>

#include "ordered_map.h"

#define ALIGNMENT (sizeof(size_t))

static inline size_t RoundUpToAlignment(size_t n)
{
    return (n + (ALIGNMENT - 1)) & ~(ALIGNMENT - 1);
}

typedef enum {
    RED,
    BLACK,
} Color;

typedef struct OrderedMapNode {
    struct OrderedMapNode *parent;
    struct OrderedMapNode *left;
    struct OrderedMapNode *right;
    Color color;
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

static inline void RotateUp(OrderedMap *map, Node *n)
{
    if (n == n->parent->left) {
        RotateRight(map, n->parent);
    } else {
        RotateLeft(map, n->parent);
    }
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

OrderedMapEntry *OrderedMap_FindEntry(const OrderedMap *map, const void *key)
{
    Node *curr = map->root;
    while (curr != NULL) {
        int cmp = map->cmp(key, NodeKey(map, curr));
        if (cmp == 0) {
            return curr;
        } else if (cmp < 0) {
            curr = curr->left;
        } else {
            curr = curr->right;
        }
    }

    return NULL;
}

OrderedMapEntry *OrderedMap_MaxEntry(const OrderedMap *map)
{
    Node *curr = map->root;
    while (curr != NULL && curr->right != NULL) {
        curr = curr->right;
    }
    return curr;
}

static void ReplaceNode(OrderedMap *map, Node *replacee, Node *replacer)
{
    if (replacer != NULL) {
        replacer->parent = replacee->parent;
    }

    if (replacee->parent == NULL) {
        assert(replacee == map->root);
        map->root = replacer;
    } else if (replacee == replacee->parent->left) {
        replacee->parent->left = replacer;
    } else {
        assert(replacee == replacee->parent->right);
        replacee->parent->right = replacer;
    }
}

static inline Node *GetSibling(Node *n, Node *parent)
{
    if (n == parent->left) {
        return parent->right;
    } else {
        return parent->left;
    }
}

static inline Color NodeColor(Node *node)
{
    if (node == NULL) {
        // NULL nodes are considered black.
        return BLACK;
    } else {
        return node->color;
    }
}

bool OrderedMap_Remove(OrderedMap *map, const void *key, void *value)
{
    Node *curr = OrderedMap_FindEntry(map, key);
    if (curr == NULL) {
        return false;
    }

    if (value != NULL) {
        memcpy(value, NodeValue(map, curr), map->value_size);
    }

    if (curr->left != NULL && curr->right != NULL) {
        // The algorithm below handles deletion of nodes with 0 or 1 child. If
        // the node we are deleting has 2 children, we reduce it to the 0- or 1-
        // child case by finding the in-order successor of `curr` (which is the
        // leftmost node in curr's right subtree, and therefore does not have a
        // left child), moving its data to `curr`, and then deleting the
        // successor.
        Node *succ = curr->right;
        while (succ->left != NULL) {
            succ = succ->left;
        }
        memcpy(NodeKey(map, curr), NodeKey(map, succ), map->key_size);
        memcpy(NodeValue(map, curr), NodeValue(map, succ), map->value_size);
        curr = succ;
    }

    // Now `curr` has at most 1 child. Select one of them arbitrarily and
    // replace `curr` with the selected child. Note that the selected child may
    // be NULL, but only if both of `curr`s children ar NULL.
    Node *child;
    if (curr->left == NULL) {
        child = curr->right;
    } else {
        assert(curr->right == NULL);
        child = curr->left;
    }
    ReplaceNode(map, curr, child);

    if (curr->color == RED) {
        // If we removed a red node, we could not have broken any of the tree
        // invariants. We can't have created a double red by removing one, and
        // we can't have changed the number of black nodes on any path by
        // removing a red node. So, we don't have to do any fixup.
        assert(NodeColor(child) == BLACK);
        free(curr);
    } else if (child != NULL) {
        // If the node we removed was black and the child we replaced it with is
        // non-NULL, then the child must be red:
        assert(NodeColor(child) == RED);
        // because otherwise, paths through `curr` would have at least one more
        // black node on the side of `child` than on the other side (since at
        // most one of `curr`s children was non-NULL).
        //
        // In this case, we decreased the number of black nodes by 1 in all
        // paths through `curr`, but all paths that used to pass through `curr`
        // now pass through `child`, so we can fix this by simply making child
        // black.
        child->color = BLACK;
        free(curr);
    } else {
        // In the case where we removed a black node and the child we replaced
        // it with is also black (or NULL) then the fixup is complicated and
        // requires us to walk up the tree some ways.

        // We'll need to keep track of the parent node as we walk up the tree,
        // but remember that `child` might be NULL, so we can't get it from
        // `child->parent`. But we know `curr` is not NULL and, even though it
        // has been removed from the tree, its parent pointer is still valid.
        Node *parent = curr->parent;

        // `curr` points to a node that is no longer in the tree, so free that
        // node's memory and begin our traversal from the node that replaced it.
        free(curr);
        curr = child;

        while (parent != NULL) {
            assert(NodeColor(curr) == BLACK);
                // Loop invariant.

            Node *sibling = GetSibling(curr, parent);
            assert(sibling != NULL);
                // Loop invariant. We know this holds for the first iteration
                // because the node we deleted and its child were both black, so
                // that subtree had a black height of at least 2. If `sibling`
                // were NULL, its black height would be only 1.

            // We have the following situation:
            //
            //        parent-> *                                              //
            //                / \                                             //
            //       curr -> b   * <- sibling                                 //
            //
            // and we know paths through `curr` have one too few black nodes.
            // We will address this in several cases, each one either making a
            // small adjustment and falling through to the next, or continuing
            // the loop higher up the tree.
            if (NodeColor(sibling) == RED) {
                // If `sibling` is red, we perform the following recoloring and
                // rotation:
                //                           b                                //
                //          b               / \                               //
                //         / \      =>     r   b                              //
                //     -> b   r           / \                                 //
                //           / \      -> b   b                                //
                //          b   b
                //
                // This transforms the situation to the next case (where both
                // `curr` and `sibling` are black) and does not change the
                // number of black nodes in paths through `curr`, so we fall
                // through.
                assert(NodeColor(parent) == BLACK);
                    // If `sibling` is red, `parent` must be black, or else we
                    // would have consecutive red nodes.

                parent->color = RED;
                sibling->color = BLACK;
                RotateUp(map, sibling);

                sibling = GetSibling(curr, parent);
                    // `curr`s sibling has changed.
            } else if (NodeColor(parent) == BLACK &&
                       NodeColor(sibling) == BLACK &&
                       NodeColor(sibling->left) == BLACK &&
                       NodeColor(sibling->right) == BLACK)
            {
                // If `parent`, `sibling`, and `sibling`s children are black, we
                // recolor `sibling`:
                //
                //
                //          b               b                                 //
                //         / \             / \                                //
                //     -> b   b        -> b   r                               //
                //           / \             / \                              //
                //          b   b           b   b                             //
                //
                // This decreases the number of black nodes in all paths through
                // `sibling`, so now all paths through `sibling` or `curr` (that
                // is, all paths through `parent`) have the same number of black
                // nodes, which is one fewer than other paths. Thus, we can move
                // up the tree and start fixing paths through `parent`.
                //
                // Note that our loop invariants will hold for the next
                // iteration:
                //  * `parent` (which will become the new `curr` stays black).
                //  * `parent`s sibling cannot be NULL, because the black height
                //    of parent is at least 2, and this is one too small, so the
                //    black height of `parent`s sibling must be at least 3.
                sibling->color = RED;
                curr = parent;
                parent = curr->parent;
                continue;
            }

            // If we reach here, then, whether naturally or by the fall-through
            // adjustment above, we know:
            assert(NodeColor(curr) == BLACK);
            assert(NodeColor(sibling) == BLACK);
            // And at least one of the nodes in question is red:
            assert(NodeColor(parent) == RED ||
                   NodeColor(sibling->left) == RED ||
                   NodeColor(sibling->right) == RED);
            // otherwise we would have continued in the previous case. So we
            // have:
            //
            //          b                                                     //
            //         / \                                                    //
            //        *   b                                                   //
            //       / \                                                      //
            //   -> b   b                                                     //
            //         / \                                                    //
            //        *   *                                                   //
            //
            // Where at least one * is red.
            if (NodeColor(parent) == RED &&
                NodeColor(sibling->left) == BLACK &&
                NodeColor(sibling->right) == BLACK)
            {
                // In this situation (which is a sub-case of the above):
                //
                //          b                                                 //
                //         / \                                                //
                //        r   b                                               //
                //       / \                                                  //
                //   -> b   b                                                 //
                //         / \                                                //
                //        b   b                                               //
                //
                // we can finish immediately by swapping the colors of `sibling`
                // and `parent`:
                //
                //          b                                                 //
                //         / \                                                //
                //        b   b                                               //
                //       / \                                                  //
                //   -> b   r                                                 //
                //         / \                                                //
                //        b   b                                               //
                //
                // Note how this does not create any red bubbles, but it does
                // increase the number of black nodes in paths through `curr`
                // without changing the number inf paths through `sibling`. This
                // fully restores our invariant.
                sibling->color = RED;
                parent->color = BLACK;
                break;
            }

            // If we don't fall into the previous case, then we know at least
            // one of `sibling`s children is red. If they were both black, then
            // parent cannot be red (that's the case above) or black (since at
            // least one of the three is red), which is a contradiction.
            //
            // First we ensure that the red child of `sibling` is on the same
            // side of `sibling` as `sibling` is of parent. We want one of the
            // following:
            //
            //        *                           *                           //
            //       / \                         / \                          //
            //   -> b   b           or          b   b <-                      //
            //         / \                     / \                            //
            //        *   r                   r   *                           //
            if (sibling == parent->left && NodeColor(sibling->left) == BLACK) {
                assert(NodeColor(sibling->right) == RED);
                // Rotate and recolor to transform:
                //
                //        *               *                                   //
                //       / \             / \                                  //
                //      b   b <-    =>  b   b <-                              //
                //     / \             /                                      //
                //    b   r           r                                       //
                //                   /                                        //
                //                  b                                         //
                //
                // Notice that this does not change the number of black nodes in
                // any path through `curr` (since its subtree is not affected)
                // or through its new sibling compared to through its old
                // sibling.
                //
                sibling->right->color = BLACK;
                sibling->color = RED;
                RotateLeft(map, sibling);

                sibling = sibling->parent;
                    // `sibling`s new parent (its old right child) is the new
                    // sibling of `curr`.
            } else if (sibling == parent->right &&
                       NodeColor(sibling->right) == BLACK)
            {
                assert(NodeColor(sibling->left) == RED);
                // Mirage image of the above:
                //
                //       *            *                                       //
                //      / \          / \                                      //
                //  -> b   b    =>  b   b                                     //
                //        / \            \                                    //
                //       r   b            r                                   //
                //                         \                                  //
                //                          b                                 //
                //
                sibling->left->color = BLACK;
                sibling->color = RED;
                RotateRight(map, sibling);

                sibling = sibling->parent;
                    // `sibling`s new parent (its old left child) is the new
                    // sibling of `curr`.
            }

            // Now we have the following:
            //       *                                                        //
            //      / \                                                       //
            //  -> b   b                                                      //
            //          \                                                     //
            //           r                                                    //
            // or its mirror image.
            if (sibling == parent->right) {
                // Perform the following rotation and recoloring:
                //       *            *                                       //
                //      / \          / \                                      //
                //  -> b   b    =>  b   b                                     //
                //          \      /                                          //
                //           r    b                                           //
                //
                // This does not change the number of black nodes in paths
                // through the new parent's right subtree, but it increases the
                // number of black nodes in paths through `curr`, which was our
                // goal, so we can break from the loop.
                RotateLeft(map, parent);
                sibling->color = parent->color;
                parent->color = BLACK;
                sibling->right->color = BLACK;
                break;
            } else {
                // Do the mirror image of above.
                RotateRight(map, parent);
                sibling->color = parent->color;
                parent->color = BLACK;
                sibling->left->color = BLACK;
                break;
            }
        }
    }

    return true;
}
