#include "internal/blimp.h"
#include "internal/vtable.h"

typedef struct {
    const Symbol *receiver;
    const Symbol *message;
} SymbolPair;

typedef struct {
    Method func;
    void *data;
} BoundMethod;

static bool SymbolPairEq(const SymbolPair *p1, const SymbolPair *p2, void *arg)
{
    (void)arg;

    return p1->receiver == p2->receiver && p1->message == p2->message;
}

static size_t SymbolPairHash(const SymbolPair *p, void *arg)
{
    (void)arg;

    return 3*p->receiver->hash + p->message->hash;
        // To combine the hashes of the two symbols, we use the formula
        //      3*hash_1 + hash_2.
        // This is similar to combining hashes using XOR, in that using `+` to
        // combine hashes produces a roughly uniform distribution as long as the
        // input hashes come from roughly uniform distributions, which is the
        // property we want in a good hash function.
        //
        // Using `+` is slightly better than XOR; the two functions have very
        // similar truth tables, but the carry bits in the `+` function preserve
        // information that is lost with XOR. For example, if the XOR of two
        // bits is a 1, we've lost whether the two input bits were both 1 or
        // both 0. With `+`, this information is carried and fed into the
        // computation of the next bit in the form of a 1 carry bit if the
        // inputs were 1, and a 0 carry bit if the inputs were 0. A concrete,
        // practical example of why this is important for our use case is that
        // XOR maps two equal inputs to 0, but `+` does not. It is general not
        // good if a hash combiner maps many different pairs of inputs to 0.
        //
        // We multiple the left-hand side by 3 to break the symmetry of the `+`
        // operator. The symbol pairs (a, b) and (b, a) are not equivalent with
        // respect to SymbolPairEq, so we want them to have different hashes.
        // The choice of 3, and the choice to mulitple on the left-hand side
        // instead of the right-hand side, are arbitrary. Any odd constant
        // multiplied with either operand will do. 3 is a reasonable choice
        // because it is one greater than a power of two, so 3x can be computed
        // with a shift and an add ((x<<1) + x) if the compiler so chooses.
}

static inline Blimp *GetBlimp(const VTable *vtable)
{
    return HashMap_GetBlimp(&vtable->methods);
}

Status VTable_Init(Blimp *blimp, VTable *vtable)
{
    // Get all of the symbols which have special meaning in the default vtable.
    const Symbol *symbol, *get, *set, *store, *eval;
    TRY(Blimp_GetSymbol(blimp, "_", &vtable->wildcard));
    TRY(Blimp_GetSymbol(blimp, "symbol", &symbol));
    TRY(Blimp_GetSymbol(blimp, ".get", &get));
    TRY(Blimp_GetSymbol(blimp, ":=", &set));
    TRY(Blimp_GetSymbol(blimp, "<-", &store));
    TRY(Blimp_GetSymbol(blimp, ".eval", &eval));

    vtable->methods.capacity = 0;
    TRY(HashMap_Init(
        blimp, &vtable->methods,
        sizeof(SymbolPair), sizeof(BoundMethod),
        (EqFunc)SymbolPairEq, (HashFunc)SymbolPairHash,
        NULL
    ));

    // Bind the primitive method symbol .get.
    if (VTable_Bind(vtable, symbol, get, BlimpMethod_PrimitiveGet, NULL)
            != BLIMP_OK)
    {
        VTable_Destroy(vtable);
        return Blimp_Reraise(blimp);
    }

    // Bind the primitive method symbol :=.
    if (VTable_Bind(vtable, symbol, set, BlimpMethod_PrimitiveSet, NULL)
            != BLIMP_OK)
    {
        VTable_Destroy(vtable);
        return Blimp_Reraise(blimp);
    }

    // Bind the primitive method symbol <-.
    if (VTable_Bind(vtable, symbol, store, BlimpMethod_PrimitiveStore, NULL)
            != BLIMP_OK)
    {
        VTable_Destroy(vtable);
        return Blimp_Reraise(blimp);
    }

    // Bind the primitive method _ .eval.
    if (VTable_Bind(
            vtable,
            vtable->wildcard,
            eval,
            BlimpMethod_PrimitiveEval,
            NULL
        )!= BLIMP_OK)
    {
        VTable_Destroy(vtable);
        return Blimp_Reraise(blimp);
    }

    return BLIMP_OK;
}

PRIVATE void VTable_Destroy(VTable *vtable)
{
    HashMap_Destroy(&vtable->methods);
}

PRIVATE Status VTable_Bind(
    VTable *vtable,
    const Symbol *receiver,
    const Symbol *message,
    const Method func,
    void *data)
{
    const SymbolPair  key   = { receiver, message };
    const BoundMethod value = { func,     data    };
    return HashMap_Update(&vtable->methods, &key, &value);
}

PRIVATE Status VTable_Resolve(
    const VTable *vtable,
    const Symbol *receiver,
    const Symbol *message,
    Method *func,
    void **data)
{
    SymbolPair key;

    key = (SymbolPair) { receiver, message };
    BoundMethod *method = HashMap_Find(&vtable->methods, &key);

    if (method == NULL && message != vtable->wildcard) {
        key = (SymbolPair) { receiver, vtable->wildcard };
        method = HashMap_Find(&vtable->methods, &key);
    }

    if (method == NULL && receiver != vtable->wildcard) {
        key = (SymbolPair) { vtable->wildcard, message };
        method = HashMap_Find(&vtable->methods, &key);
    }

    if (method == NULL) {
        return RuntimeErrorMsg(
            GetBlimp(vtable), BLIMP_NO_SUCH_METHOD,
            "no method bound for `%s %s'", receiver->name, message->name);
    }

    if (func) {
        *func = method->func;
    }
    if (data) {
        *data = method->data;
    }
    return BLIMP_OK;
}

