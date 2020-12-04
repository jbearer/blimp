#include <assert.h>

#include "hash_map.h"
#include "system.h"

static size_t HashMap_SymbolHash(const BlimpSymbol **sym, void *arg)
{
    (void)arg;
    return BlimpSymbol_Hash(*sym);
}

static bool HashMap_SymbolEq(
    const BlimpSymbol **sym1, const BlimpSymbol **sym2, void *arg)
{
    (void)arg;
    return *sym1 == *sym2;
}

BlimpStatus Function(
    Blimp *blimp, const char *name, BlimpMethod method, void *arg)
{
    const BlimpSymbol *symbol;
    BlimpObject *function;
    if (Blimp_GetSymbol(blimp, name, &symbol) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    if (BlimpObject_NewExtension(
            blimp, Blimp_GlobalObject(blimp), arg, method, NULL, &function)
        != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    if (BlimpObject_Set(
            Blimp_GlobalObject(blimp), symbol, function) != BLIMP_OK)
    {
        BlimpObject_Release(function);
        return Blimp_Reraise(blimp);
    }
    BlimpObject_Release(function);
    return BLIMP_OK;
}

BlimpStatus VoidReturn(Blimp *blimp, BlimpObject **result)
{
    BlimpObject_Borrow(Blimp_GlobalObject(blimp));
    *result = Blimp_GlobalObject(blimp);
    return BLIMP_OK;
}

typedef struct {
    const Class *cls;
    void *state;
} ClassInstance;

// A partially applied method call. The MethodCall struct is used to collect
// arguments to a method until there are enough, and then apply the method.
typedef struct {
    const Method *method;
    size_t args_received;
    void *state;
    BlimpObject *args[];
} MethodCall;

static BlimpStatus MethodCall_ReceiveArg(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)context;

    MethodCall *call;
    if (BlimpObject_ParseExtension(receiver, NULL, (void **)&call) != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    assert(call->args_received < call->method->num_args);
    call->args[call->args_received++] = BlimpObject_Borrow(message);
    if (call->args_received == call->method->num_args) {
        // If we have enough arguments, call the method handler and return the
        // result.
        BlimpStatus status = call->method->call(
            blimp, call->args, call->state, result);

        // Release our references to the method's arguments.
        for (size_t i = 0; i < call->method->num_args; ++i) {
            BlimpObject_Release(call->args[i]);
        }

        return status;
    } else {
        // If we still don't have enough arguments, return the same MethodCall
        // object so we can collect more.
        *result = BlimpObject_Borrow(receiver);
        return BLIMP_OK;
    }
}

// An object which will evaluate to the result of calling `method` once it has
// received enough arguments.
static BlimpStatus MethodCaller(
    Blimp *blimp, const Method *method, void *state, BlimpObject **result)
{
    if (method->num_args == 0) {
        // If `method` does not require any arguments, call it and return the
        // result directly.
        return method->call(blimp, NULL, state, result);
    }
    // Otherwise, we'll return a MethodCall extension object which will collect
    // the proper number of arguments and then call the method.

    MethodCall *call = malloc(
        sizeof(MethodCall) + method->num_args*sizeof(BlimpObject *));
    if (call == NULL) {
        return Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY);
    }
    call->method = method;
    call->state = state;
    call->args_received = 0;

    if (BlimpObject_NewExtension(
            blimp,
            Blimp_GlobalObject(blimp),
            call,
            MethodCall_ReceiveArg,
            free,
            result)
        != BLIMP_OK)
    {
        free(call);
        return Blimp_Reraise(blimp);
    }

    return BLIMP_OK;
}

// Access the method `message` (which must be a symbol) of the object `receiver`
// (which must be an instance of an extension class).
static BlimpStatus Instance_Dispatch(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)context;

    ClassInstance *instance;
    if (BlimpObject_ParseExtension(receiver, NULL, (void **)&instance)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    const BlimpSymbol *method_name;
    if (BlimpObject_ParseSymbol(message, &method_name) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    Method *method = HashMap_Find(&instance->cls->vtable, &method_name);
    if (method == NULL) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "no such method %s of class %s",
            BlimpSymbol_GetName(method_name),
            BlimpSymbol_GetName(instance->cls->name));
    }

    return MethodCaller(blimp, method, instance->state, result);
}

static BlimpStatus Class_MakeInstance(
    Blimp *blimp,
    BlimpObject **args,
    void *state,
    BlimpObject **result)
{
    const Class *cls = (const Class *)state;
    if (cls->constr.call(blimp, args, &state) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return Instance(blimp, cls, state, result);
}

// Call a constructor to create an instance of an extension class. The first
// constructor argument is `receiver`; subsequent arguments will be collected in
// curried fashion.
static BlimpStatus Class_Constructor(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    const Class *cls;
    if (BlimpObject_ParseExtension(receiver, NULL, (void **)&cls)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    BlimpObject *constr_caller;
    if (MethodCaller(blimp, &cls->constr_method, (void *)cls, &constr_caller)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    if (Blimp_Send(
            blimp, context, constr_caller, message, result)
        != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    return BLIMP_OK;
}

BlimpStatus Class_Init(
    Blimp *blimp,
    Class *cls,
    const char *class_name,
    const Constructor *constr,
    const Method *methods)
{
    cls->constr = *constr;

    // Create a Method to represent the constructor so that we can use the
    // MethodCaller() mechanism to handle curried calls to the constructor. The
    // handler function will be Class_MakeInstance(), which forwards the
    // arguments to the Constructor itself and uses the `state` returned by the
    // Constructor to create a new instance.
    cls->constr_method = (Method) {
        .name = NULL,
        .num_args = constr->num_args,
        .call = Class_MakeInstance,
    };

    if (Blimp_GetSymbol(blimp, class_name, &cls->name) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    if (HashMap_Init(
            blimp, &cls->vtable, sizeof(BlimpSymbol *), sizeof(Method),
            (EqFunc)HashMap_SymbolEq, (HashFunc)HashMap_SymbolHash, NULL)
        != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    // Add each method to the vtable.
    for (const Method *method = methods; method->name != NULL; ++method) {
        const BlimpSymbol *name;
        if (Blimp_GetSymbol(blimp, method->name, &name) != BLIMP_OK) {
            HashMap_Destroy(&cls->vtable);
            return Blimp_Reraise(blimp);
        }
        if (HashMap_Update(&cls->vtable, &name, method) != BLIMP_OK) {
            HashMap_Destroy(&cls->vtable);
            return Blimp_Reraise(blimp);
        }
    }

    // Register a global function to invoke the constructor.
    if (Function(blimp, class_name, Class_Constructor, cls) != BLIMP_OK) {
        HashMap_Destroy(&cls->vtable);
        free(cls);
        return Blimp_Reraise(blimp);
    }

    return BLIMP_OK;
}

BlimpStatus Instance(
    Blimp *blimp, const Class *cls, void *state, BlimpObject **result)
{
    ClassInstance *instance = malloc(sizeof(ClassInstance));
    if (instance == NULL) {
        return Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY);
    }
    instance->cls = cls;
    instance->state = state;

    if (BlimpObject_NewExtension(
            blimp,
            Blimp_GlobalObject(blimp),
            instance,
            Instance_Dispatch,
            free,
            result)
        != BLIMP_OK)
    {
        free(instance);
        return Blimp_Reraise(blimp);
    }

    return BLIMP_OK;
}

BlimpStatus ParseInstance(
    Blimp *blimp, const Class *cls, BlimpObject *obj, void **state)
{
    ClassInstance *instance;
    if (BlimpObject_ParseExtension(obj, NULL, (void **)&instance) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    if (instance->cls != cls) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "invalid cast from extension class %s to extension class %s",
            BlimpSymbol_GetName(instance->cls->name),
            BlimpSymbol_GetName(cls->name));
    }

    *state = instance->state;
    return BLIMP_OK;
}
