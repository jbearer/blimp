#include "system.h"

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
