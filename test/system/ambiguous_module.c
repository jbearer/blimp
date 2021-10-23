#include <blimp/module.h>

static BlimpStatus InitAmbiguousModule(
    Blimp *blimp, BlimpObject *context, BlimpObject **result)
{
    (void)context;

    const BlimpSymbol *sym;
    if (Blimp_GetSymbol(blimp, "binary", &sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    return BlimpObject_NewSymbol(blimp, sym, result);
}

BLIMP_MODULE(InitAmbiguousModule);
