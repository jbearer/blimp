#include "internal/blimp.h"
#include "internal/symbol.h"

Blimp *Blimp_New(void)
{
    Blimp *blimp = malloc(sizeof(Blimp));
    if (blimp == NULL) {
        return NULL;
    }

    if (Blimp_InitSymbolTable(blimp) != BLIMP_OK) {
        free(blimp);
        return NULL;
    }

    return blimp;
}
