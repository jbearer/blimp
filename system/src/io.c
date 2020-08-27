#include <blimp/module.h>

static inline void VoidReturn(Blimp *blimp, BlimpObject **result)
{
    BlimpObject_Borrow(Blimp_GlobalObject(blimp));
    *result = Blimp_GlobalObject(blimp);
}

static BlimpStatus Print(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)scope;
    (void)receiver;

    BlimpObject_Print(stdout, message);
    fputc('\n', stdout);

    VoidReturn(blimp, result);
    return BLIMP_OK;
}

static BlimpStatus InitSystemIO(
    Blimp *blimp, BlimpObject *context, BlimpObject **result)
{
    (void)context;

    VoidReturn(blimp, result);

    const BlimpSymbol *print;
    BlimpObject *print_method;
    if (Blimp_GetSymbol(blimp, "print", &print) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    if (BlimpObject_NewExtension(
            blimp, Blimp_GlobalObject(blimp), NULL, Print, NULL, &print_method)
        != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    if (BlimpObject_Set(
            Blimp_GlobalObject(blimp), print, print_method) != BLIMP_OK)
    {
        BlimpObject_Release(print_method);
        return Blimp_Reraise(blimp);
    }
    BlimpObject_Release(print_method);

    return BLIMP_OK;
}

BLIMP_MODULE(InitSystemIO);
