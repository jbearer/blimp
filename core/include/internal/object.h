////////////////////////////////////////////////////////////////////////////////
// The Object Hierarchy
//
// The 5 kinds of bl:mp objects (symbol, reference, global, block, and
// extension) are represented by a hierarchy of polymorphic abstract classes,
// where each more derived class encapsulates additional properties that certain
// kinds of objects might have:
//
//    Object:    Base class for all objects. This class contains only enough
//       ^       information to determine the runtime type of a derived Object
//       |       instance. This class is the immediate base class of objects
//       |       which persist for the lifetime of their owning bl:mp, and which
//       |       therefore do not require garbage collection.
//       |
//   GC_Object:  Base class which supports basic garbage collection (reference
//       ^       counting and tracing). This class does not support enhanced
//       |       reference counting. It should be used as the immediate base
//       |       class for objects which should be freed when they are no longer
//       |       in use, and which may be referenced dynamically by other
//       |       objects (so that they require garbage collection), but which do
//       |       not have any strong references to other objects (so that they
//       |       cannot participate in a cycle, and thus do not require ERC).
//       |
// ScopedObject: Base class which support outgoing, owning references. This
//               class includes a scope (a map from symbols to object
//               references) as well as a parent reference (for chained scope
//               lookups) and a stack of message objects which were captured
//               from the parent scope. Because these objects can reference
//               other objects (and be referenced by other objects) this class
//               also supports ERC for efficient garbage collection of cycles.
//
// Each of the 5 kinds of objects is a concrete class which inherits from one
// of these abstract classes and extends it with additional properties:
//
//  Symbol: Object
//      A persistent object representing a unique string. Since symbol objects
//      have no internal mutable state, they can be deduplicated: two different
//      objects of the same symbol can be represented by the same object. To
//      keep things simple and avoid extra allocations, Symbol objects are
//      identified with the Symbol type used in the AST and generated by the
//      parser. They last for the duration of the bl:mp interpreter and are not
//      garbage collected.
//
//  ReferenceObject: GC_Object
//      A reference is a garbage-collectable object which represents the
//      capability of changing the value of a symbol in a particular scope.
//      Type-specific properties include the symbol and a weak reference to the
//      scope in which the reference was created.
//
//  GlobalObject: ScopedObject
//      Each bl:mp interpreter has exactly one global object, which is just a
//      ScopedObject with no parent. It is like a BlockObject, except it cannot
//      receive messages.
//
//  BlockObject: ScopedObject
//      The result of evaluating a block expression ({...}). In addition to a
//      scope, a block object contains an expression which is evaluated when the
//      object receives a message.
//
//  ExtensionObject: ScopedObject
//      Extension objects can be used to extend the basic functionality of the
//      bl:mp interpreter by writing C programs. They are exactly like blocks,
//      except that the code which is evaluated when they receive a message is
//      written in C, not in bl:mp.
//
// FreeObject: GC_Object
//      A GC_Object which has been freed.
//
// The inheritance described here is implemented using struct composition: each
// class is represented by a struct. A class which inherits from another class
// has the base class struct as its first element. Since C allows casting
// between a pointer to a struct and a pointer to its first element, we have the
// following rules for casting:
//  * Upcasting (e.g. (Object *)(GC_Object *)(ScopedObject *)block_object) is
//    always allowed.
//  * Downcasting to an abstract class is allowed if the object is known to be
//    an instance of a subclass of the abstract class. The Is* predicates can be
//    used to determine this. For example, (ScopedObject *)object is allowed if
//    IsScopedObject(object) returns `true`.
//  * Downcasting to a concrete class is allowed if the object is known to be an
//    instance of that class. Object_Type() can be used to check this. For
//    example, (BlockObject *)object is allowed if
//    `Object_Type(object) == OBJ_BLOCK`.
//

#ifndef BLIMP_OBJECT_H
#define BLIMP_OBJECT_H

#include "internal/bit_vector.h"
#include "internal/common.h"
#include "internal/debruijn.h"
#include "internal/hash_map.h"
#include "internal/instruction.h"
#include "internal/pool_alloc.h"
#include "internal/random.h"

////////////////////////////////////////////////////////////////////////////////
// abstract class Object
//

// ObjectType
//
// Note: the order of values in this enum is important. It represents the
// subtyping relationships in the Object hierarchy, so we can efficiently test
// whether a concrete object type is an instance of an abstract type by simply
// comparing ObjectTypes.
typedef enum {
    // Object
    OBJ_SYMBOL,

    // GC_Object
    OBJ_FREE,
    OBJ_REFERENCE,

    // ScopedObject
    OBJ_GLOBAL,
    OBJ_BLOCK,
    OBJ_EXTENSION,
} ObjectType;

struct BlimpObject {
    uintptr_t blimp_and_type;
        // For convenience, every object stores a pointer to the bl:mp that owns
        // it. The object type is encoded in the low 3 bits of this field (which
        // are unused in the pointer to the bl:mp, since a Blimp object is
        // always aligned to at least an 8-byte boundary).
};

#define OBJECT_TYPE_MASK ((uintptr_t)7)

static inline void Object_Init(Object *obj, Blimp *blimp, ObjectType type)
{
    assert(!((uintptr_t)blimp & OBJECT_TYPE_MASK));
    assert(!(type & (~OBJECT_TYPE_MASK)));

    obj->blimp_and_type = (uintptr_t)blimp | type;
}

static inline Blimp *Object_Blimp(const Object *obj)
{
    return (Blimp *)(obj->blimp_and_type & (~OBJECT_TYPE_MASK));
}

static inline ObjectType Object_Type(const Object *obj)
{
    return obj->blimp_and_type & OBJECT_TYPE_MASK;
}

static inline void Object_SetType(Object *obj, ObjectType type)
{
    assert(!(type & (~OBJECT_TYPE_MASK)));
    obj->blimp_and_type = (uintptr_t)Object_Blimp(obj) | type;
}

static inline bool IsGC_ObjectType(ObjectType type)
{
    return type >= OBJ_FREE;
}

static inline bool IsScopedObjectType(ObjectType type)
{
    return type >= OBJ_GLOBAL;
}

static inline bool IsGC_Object(const Object *obj)
{
    return IsGC_ObjectType(Object_Type(obj));
}

static inline bool IsScopedObject(const Object *obj)
{
    return IsScopedObjectType(Object_Type(obj));
}

PRIVATE bool Object_IsFree(Object *obj);

////////////////////////////////////////////////////////////////////////////////
// abstract class GC_Object extends Object
//

typedef struct {
    Object header;

    // Reference counting.
    size_t transient_refcount;
        // The number of short-lived, local references to this object. This
        // accounts for users of the API who are currently working on this
        // object.
        //
        // Because this accounts for references that are not managed by the
        // garbage collector, an object with a nonzero `transient_refcount` will
        // not be freed by garbage collection even if it is unreachable. Users
        // of the API are responsible for avoiding or breaking cycles that
        // contribute to `transient_refcount`.
        //
        // This is the reference count maintained by BlimpObject_Borrow() and
        // BlimpObject_Release().
    size_t internal_refcount;
        // The number of persistent, GC-managed references to this objects. This
        // accounts for references from an object's scope to another object, and
        // for parent pointers. It is managed internally by the ObjectPool
        // module.
        //
        // Because a nonzero `internal_refcount` does not necessarily indicate
        // that anyone outside the interal object module is using an object, GC
        // will free objects with a nonzero `internal_refcount` if it can
        // determine the object is not reachable from any object with a nonzero
        // `transient_refcount`.

    bool freeing;
        // Set while the object is being freed, but still might be reachable.
        //
        // When in ERC is enabled, reference counting is able to free cyclic
        // references, which means freeing one object may cause is to free
        // another which is already in the process of being freed. For example:
        //
        //      --> A <-> B
        //
        // When we release the external reference to A, we may detect that the
        // A <-> B cycle can be freed, so we free A. In the process of freeing
        // A, B's reference count drops to 0, so we free B by normal reference
        // counting. That causes A's reference count to drop to 0, so we free A,
        // resulting in a loop.
        //
        // This flag lets us detect such would-be loops, and short-circuit the
        // second time we free A, since we know the job of freeing A is already
        // taken care of farther up the call stack.

    // Reachability analysis.
    bool reached;
        // This field should be false for all objects except during a
        // reachability traversal, where it is used intermittently to mark
        // objects which have already been visited in the traversal. Any code
        // doing such a traversal is responsible for resetting all of the
        // `reached` flags to false afterwards.
        //
        // This should never be set on a free object.
} GC_Object;

////////////////////////////////////////////////////////////////////////////////
// abstract class ScopedObject extends GC_Object
//

typedef HashMap Scope;

typedef struct Ref {
    Object *to;
    bool reserved;
    bool is_const;
    struct Ref *next;
    struct Ref *prev;
} Ref;

typedef struct ScopedObject {
    GC_Object header;

    struct ScopedObject *parent;

    // Entanglement cycle detection.
    size_t seq;
    bv128 predecessors;
        // Bloom filter of objects reachable from this object by following
        // parent pointers, not including this object.
    bv128 self_mask;
        // Mask corresponding to this object in a Bloom filter.
    struct ScopedObject *entangled;
        // The object that this object is entangled with, or NULL if this is
        // the representative of its clump. If non-NULL, `entangled` is
        // guaranteed to be a predecessor of this object. The representative of
        // each clump is reachable from any object in the clump by following
        // `entangled` pointers.
    size_t clump_refcount;
        // Number of external references to this clump. This is only meaningful
        // for the representative of a clump (`entangled == NULL`).
    struct ScopedObject *clump_next;
    struct ScopedObject *clump_prev;
        // Doubly linked, circular list of all the objects in this clump.
    Ref *clump_references;
        // List of references from this clump to other clumps.

    Scope scope;
    DeBruijnMap captures;
        // Stack of Refs to message objects which the body of this object is
        // closed over. This does not include the message currently being
        // processed by this object (which is represented by the top of the
        // bl:mp call stack) because the same object instance can be sent many
        // messages at different times (or even at the same time in the presence
        // of recursion) so it does not make sense to store this message with
        // the object itself.
        //
        // However, even though the parent of this object may be sent many
        // different messages during its life this object was created while
        // exactly one of those messages was in scope, and remains closed over
        // that particular message for the duration of its life.
        //
        // Each object is closed over all of the objects its parent is closed
        // over, plus the message in the top stack frame at the time it is
        // created.
        //
        // Note that this field only applies for object types with custom
        // message handlers (OBJ_BLOCK and OBJ_EXTENSION). Other object types
        // have built-in message handlers which do not refer to captured
        // messages from their parent, so this field is not used.
    struct ScopedObject *next;
        // The next pointer in a stack used for depth-first traversal of the
        // object pool during reachability analysis.
} ScopedObject;

PRIVATE Status ScopedObject_Set(
    ScopedObject *obj, const Symbol *sym, Object *value);
PRIVATE bool ScopedObject_Lookup(
    ScopedObject *obj,
    const Symbol *sym,
    Object **value,
    ScopedObject **owner,
    bool *is_const);
PRIVATE Status ScopedObject_Get(
    ScopedObject *obj, const Symbol *sym, Object **value);
PRIVATE Status ScopedObject_GetCapturedMessage(
    const ScopedObject *obj, size_t index, Object **message);
PRIVATE Status ScopedObject_GetCapturedMessageByName(
    const ScopedObject *obj, const Symbol *name, Object **message);

////////////////////////////////////////////////////////////////////////////////
// concrete class Symbol extends Object
//

struct BlimpSymbol {
    Object header;
    const char *name;
    size_t hash;
    size_t length;
};

////////////////////////////////////////////////////////////////////////////////
// concrete class ReferenceObject extends GC_Object
//

typedef struct {
    GC_Object header;
        // This object type does not have any outgoing strong references.
        // Therefore, it can never participate in a cycle, so enhanced reference
        // counting does not apply. We do need regular garbage collection,
        // though, because this object can be referenced from other objects
        // dynamically.

    const Symbol *symbol;

    ScopedObject *scope;
        // Weak reference to the object in whose scope to set the value of
        // `symbol` when this reference object receives a message.
        //
        // This reference does not need to extend the life of the object it
        // points to as long as we can detect if the referenced object is dead,
        // because if the scope captured by this reference is only reachable
        // through this reference, then the value of `symbol` in that scope can
        // be set, but never read. It follows that setting the value would have
        // no effect, and so treating messages received by this reference as
        // no-ops is conformant behavior.
        //
        // We detect if the `scope` object is dead using the type of the object,
        // and the `scope_seq` field below. First note that once a memory region
        // has been initialized as an Object, it remains an Object for the
        // duration of the lifetime of the owning bl:mp, so it is always safe to
        // dereference `scope` from a C language memory safety standpoint.
        // Second, note that while `scope` is live, `scope->type` is always one
        // of OBJ_GLOBAL, OBJ_BLOCK, or OBJ_EXTENSION, as these are the only
        // object types which can execute code in their scope, necessary to
        // create a reference object which captures their scope.
        //
        // If the captured scope is OBJ_GLOBAL, then it persists for the
        // lifetime of the bl:mp, so this weak reference is always live.
        // Further, only one object will ever have the OBJ_GLOBAL type, so we
        // can determine with certainty if we are in this case by checking if
        // `scope->type == OBJ_GLOBAL`.
        //
        // Otherwise, the captured scope is OBJ_BLOCK or OBJ_EXTENSION. Both of
        // these object types have a sequence number which persists for the
        // lifetime of the bl:mp and increases monotonically each time the
        // memory is reused for a new object. The sequence number for the
        // captured scope is saved in `scope_seq` when the weak reference is
        // created. Thereafter, we can determine if the weak reference is live
        // by checking if `((BlockObject)scope)->seq == scope_seq`.
    size_t scope_seq;

    bool unique;
} ReferenceObject;

PRIVATE Status ReferenceObject_New(
    Blimp *blimp,
    ScopedObject *scope,
    const Symbol *symbol,
    ReferenceObject **object);
PRIVATE Status ReferenceObject_Store(ReferenceObject *ref, Object *value);
PRIVATE void ReferenceObject_Freeze(ReferenceObject *ref);

////////////////////////////////////////////////////////////////////////////////
// concrete class GlobalObject extends ScopedObject
//

typedef struct {
    ScopedObject header;
} GlobalObject;

PRIVATE Status GlobalObject_New(Blimp *blimp, GlobalObject **object);

////////////////////////////////////////////////////////////////////////////////
// concrete class BlockObject extends ScopedObject
//

typedef struct {
    ScopedObject header;

    const Symbol *msg_name;
        // Name of the message bound in this block. This is only used for
        // pretty-printing. Elsewhere, messages are referred to exclusively by
        // DeBruijn index.
    Bytecode *code;
        // Body of the block.
    size_t specialized_seq;
        // Sequence number of the scope in which this object's code is
        // specialized. The object with this sequence number is an ancestor of
        // this object (that is, this object itself, or its parent, or its
        // parent's parent, etc.).
} BlockObject;

PRIVATE Status BlockObject_New(
    Blimp *blimp,
    ScopedObject *parent,
    const Symbol *msg_name,
    Bytecode *code,
    bool capture_parents_message,
    size_t specialized,
    BlockObject **object);

/**
 * \brief Determine if a block's code is already specialized in a given scope.
 *
 * Returns `true` if and only if `obj` is specialized in `scope` or a descendant
 * of `scope`.
 *
 * \pre `scope` is an ancestor of `obj`.
 */
static inline bool BlockObject_IsSpecialized(
    BlockObject *obj, ScopedObject *scope)
{
    return scope->seq <= obj->specialized_seq;
        // Since `scope` and the scope in which `obj` is specialized are both
        // ancestors of `obj`, they must be related; that is, one is an ancestor
        // of the other. Therefore, we can determine if `scope` is an ancestor
        // of the specialization scope by comparing sequence numbers, using the
        // property that an ancestor's sequence number is always less than its
        // descendants'.
}

/**
 * \brief Specialize a block's code for the scope of one of its ancestors.
 *
 * If `obj` is already specialized in `scope`, this has no effect. Otherwise,
 * the code of `obj`, `scope`, and every object in between is updated to new
 * procedures which are only executed in descendants of `scope`, and which are
 * optimized accordingly.
 *
 * \pre `scope` is an ancestor of `obj`.
 */
PRIVATE Status BlockObject_Specialize(BlockObject *obj, ScopedObject *scope);

////////////////////////////////////////////////////////////////////////////////
// concrete class ExtensionObject extends ScopedObject
//

typedef struct {
    ScopedObject header;

    BlimpMethod method;
    BlimpFinalizer finalize;
    void *state;
} ExtensionObject;

PRIVATE Status ExtensionObject_New(
    Blimp *blimp,
    ScopedObject *parent,
    void *state,
    BlimpMethod method,
    BlimpFinalizer finalize,
    ExtensionObject **object);

////////////////////////////////////////////////////////////////////////////////
// ObjectPool
//
// An ObjectPool is an allocator for Objects, optimized for fast creation,
// destruction, and reuse of short-lived objects, which are very common in bl:mp
// programs. See the comments in pool_alloc.h and object.c for details on the
// implementation.
typedef struct {
    PoolAllocator reference_object_pool;
        // Allocator for ReferenceObject instances.
    PoolAllocator scoped_object_pool;
        // Allocator for both kinds of dynamically allocated scoped objects
        // (blocks and extensions). These objects are approximately the same
        // size and are used in exactly the same ways (the only difference being
        // what user-written code gets evaluated when the object receives a
        // message) so it makes sense to allocate them from the same pool. This
        // avoids a regrettable situation where we have one half-full batch from
        // each of two different pools, instead of one full batch from the
        // shared pool.
        //
        // In addition, both of these objects inherit from ScopedObject, so we
        // can save on initialization and teardown costs by preinitializing the
        // ScopedObject base class portion of each object the first time it is
        // allocated and then never fully tearing it down.
        //
        // To be perfectly precise, this pool allocates ScopedObject instances
        // whose size is the maximum size of BlockObject and ExtensionObject.
        // Initialization of the fields of the derived class is performed
        // separately depending on whether the object being created is a
        // BlockObject or an ExtensionObject.
    PoolAllocator ref_pool;
        // Allocator for Refs.

    Random random;
    size_t seq;
    size_t gc_collections;
} ObjectPool;

PRIVATE Status ObjectPool_Init(Blimp *blimp, ObjectPool *pool);
PRIVATE void ObjectPool_Destroy(ObjectPool *pool);
PRIVATE void ObjectPool_CollectGarbage(ObjectPool *pool);
PRIVATE BlimpGCStatistics ObjectPool_GetStats(ObjectPool *pool);
PRIVATE void ObjectPool_DumpHeap(
    FILE *f, ObjectPool *pool, bool include_reachable);


#endif
