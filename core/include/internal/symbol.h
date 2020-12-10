#ifndef BLIMP_SYMBOL_H
#define BLIMP_SYMBOL_H

#include "hash_map.h"

typedef HashMap SymbolTable;

PRIVATE Status SymbolTable_Init(Blimp *blimp, SymbolTable *symbols);
PRIVATE Status SymbolTable_GetSymbol(
    SymbolTable *symbols, const char *name, const Symbol **symbol);
PRIVATE void SymbolTable_Destroy(SymbolTable *symbols);

PRIVATE bool SymbolEq(const Symbol **sym1, const Symbol **sym2, void *arg);
PRIVATE size_t SymbolHash(const Symbol **symbol, void *arg);

#endif // BLIMP_SYMBOL_H
