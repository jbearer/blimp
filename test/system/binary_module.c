#include <blimp/module.h>

static BlimpStatus InitBinaryModule(
    Blimp *blimp, BlimpObject *context, BlimpObject **result)
{
    const BlimpSymbol *binary_symbol;
    if (Blimp_GetSymbol(blimp, "binary_symbol", &binary_symbol) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    if (BlimpObject_Get(context, binary_symbol, result) != BLIMP_OK) {
        const BlimpSymbol *sym;
        if (Blimp_GetSymbol(blimp, "foo", &sym) != BLIMP_OK) {
            return Blimp_Reraise(blimp);
        }
        return BlimpObject_NewSymbol(blimp, sym, result);
    }

    return BLIMP_OK;
}

BLIMP_MODULE(InitBinaryModule);
