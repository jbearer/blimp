#ifndef BLIMP_SYMBOL_H
#define BLIMP_SYMBOL_H

#include "internal/common.h"
#include "internal/hash_map.h"

typedef HashMap SymbolTable;

PRIVATE Status SymbolTable_Init(Blimp *blimp, SymbolTable *symbols);
PRIVATE Status SymbolTable_GetSymbol(
    SymbolTable *symbols, const char *name, const Symbol **symbol);
PRIVATE void SymbolTable_Destroy(SymbolTable *symbols);

#endif // BLIMP_SYMBOL_H
