#ifndef BLIMP_SYMBOL_H
#define BLIMP_SYMBOL_H

#include "internal/common.h"
#include "internal/hash_map.h"

struct BlimpSymbol {
    const char *name;
    size_t hash;
    size_t length;
};

typedef HashMap SymbolTable;

PRIVATE Status SymbolTable_Init(Blimp *blimp, SymbolTable *symbols);
PRIVATE Status SymbolTable_GetSymbol(
    SymbolTable *symbols, const char *name, const Symbol **symbol);
PRIVATE void SymbolTable_Destroy(SymbolTable *symbols);

PRIVATE bool SymbolEq(const Symbol **sym1, const Symbol **sym2);
PRIVATE size_t SymbolHash(const Symbol **symbol);

#endif // BLIMP_SYMBOL_H
