#include "internal/blimp.h"
#include "internal/symbol.h"

Blimp *Blimp_New(void)
{
    Blimp *blimp = malloc(sizeof(Blimp));
    if (blimp == NULL) {
        return NULL;
    }

    if (SymbolTable_Init(blimp, &blimp->symbols) != BLIMP_OK) {
        free(blimp);
        return NULL;
    }

    return blimp;
}

void Blimp_Delete(Blimp *blimp)
{
    SymbolTable_Destroy(&blimp->symbols);
    free(blimp);
}

Status Blimp_GetSymbol(Blimp *blimp, const char *name, const Symbol **symbol)
{
    return SymbolTable_GetSymbol(&blimp->symbols, name, symbol);
}
