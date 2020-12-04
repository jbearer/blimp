#include "system.h"

static const BlimpSymbol *true_sym;
static const BlimpSymbol *false_sym;

////////////////////////////////////////////////////////////////////////////////
// native_int
//
// An extension class with the same interface as the `int` type from the
// prelude. `native_int` is implemented more efficiently, using a pointer-sized
// native machine word to do arithmetic in hardware.
//
// The `native_int` constructor takes a symbol which is an integer literal (e.g.
// `native_int 42` or `native_int 0xdeadbeef`) and returns a `native_int`
// instance which responds to the standard `int` methods (e.g. +, times, etc.).
//

typedef intptr_t native_int_t;

static Class native_int;

static BlimpStatus ParseNativeInt(
    Blimp *blimp, BlimpObject *obj, native_int_t *n)
{
    return ParseInstance(blimp, &native_int, obj, (void **)n);
}

static BlimpStatus NativeInt_Init(
    Blimp *blimp, BlimpObject **args, void **state)
{
    const BlimpSymbol *sym;
    if (BlimpObject_ParseSymbol(args[0], &sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    const char *str = BlimpSymbol_GetName(sym);
    char *invalid;
    native_int_t n = strtol(str, &invalid, 0);
    if (!*str || *invalid) {
        return Blimp_ErrorMsg(
            blimp, BLIMP_ERROR,
            "native_int argument must be the name of an integer (got %s)", str);
    }

    *state = (void *)n;
    return BLIMP_OK;
}

static BlimpStatus NativeInt_Eq(
    Blimp *blimp, BlimpObject **args, void *state, BlimpObject **result)
{
    native_int_t n1 = (native_int_t)state;
    native_int_t n2;
    if (ParseNativeInt(blimp, args[0], &n2) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return BlimpObject_NewSymbol(
        blimp, n1 == n2 ? true_sym : false_sym, result);
}

static BlimpStatus NativeInt_Add(
    Blimp *blimp, BlimpObject **args, void *state, BlimpObject **result)
{
    native_int_t n1 = (native_int_t)state;
    native_int_t n2;
    if (ParseNativeInt(blimp, args[0], &n2) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return Instance(blimp, &native_int, (void *)(n1 + n2), result);
}

static BlimpStatus NativeInt_Sub(
    Blimp *blimp, BlimpObject **args, void *state, BlimpObject **result)
{
    native_int_t n1 = (native_int_t)state;
    native_int_t n2;
    if (ParseNativeInt(blimp, args[0], &n2) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return Instance(blimp, &native_int, (void *)(n1 - n2), result);
}

static BlimpStatus NativeInt_Mul(
    Blimp *blimp, BlimpObject **args, void *state, BlimpObject **result)
{
    native_int_t n1 = (native_int_t)state;
    native_int_t n2;
    if (ParseNativeInt(blimp, args[0], &n2) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return Instance(blimp, &native_int, (void *)(n1 * n2), result);
}

static BlimpStatus NativeInt_Times(
    Blimp *blimp, BlimpObject **args, void *state, BlimpObject **result)
{
    native_int_t n = (native_int_t)state;

    for (native_int_t i = 0; i < n; ++i) {
        if (Blimp_Send(
                blimp, Blimp_GlobalObject(blimp), args[0], args[0], NULL)
            != BLIMP_OK)
        {
            return Blimp_Reraise(blimp);
        }
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus InitSystemTypes(
    Blimp *blimp, BlimpObject *context, BlimpObject **result)
{
    (void)context;

    if (Blimp_GetSymbol(blimp, "true", &true_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    if (Blimp_GetSymbol(blimp, "false", &false_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    if (Class_Init(blimp, &native_int, "native_int",
            &(Constructor){1, NativeInt_Init},
            (Method[]){
                {"==", 1, NativeInt_Eq},
                {"+", 1, NativeInt_Add},
                {"-", 1, NativeInt_Sub},
                {"*", 1, NativeInt_Mul},
                {"times", 1, NativeInt_Times},
                {0}
            })
        != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    return VoidReturn(blimp, result);
}

BLIMP_MODULE(InitSystemTypes);
