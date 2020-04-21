#include "internal/blimp.h"
#include "internal/expr.h"
#include "internal/error.h"
#include "internal/symbol.h"

Status Blimp_InitSymbolTable(Blimp *blimp)
{
    blimp->symbols.size = 0;
    blimp->symbols.capacity = 16;
    return Malloc(
        blimp, sizeof(Symbol *)*blimp->symbols.capacity, &blimp->symbols.symbols);
}

Status Blimp_GetSymbol(Blimp *blimp, const char *name, const Symbol **symbol)
{
    for (size_t i = 0; i < blimp->symbols.size; ++i) {
        if (strcmp(name, blimp->symbols.symbols[i]->name) == 0) {
            *symbol = blimp->symbols.symbols[i];
            return BLIMP_OK;
        }
    }

    if (blimp->symbols.size >= blimp->symbols.capacity) {
        TRY(Realloc(blimp, sizeof(Symbol *)*2*blimp->symbols.capacity,
            &blimp->symbols.symbols));
        blimp->symbols.capacity *= 2;
    }

    // Create a new symbol, but don't put it anywhere until we succeed.
    Symbol *new_symbol;
    TRY(Malloc(blimp, sizeof(Symbol), &new_symbol));
    new_symbol->length = strlen(name);
    TRY(Strndup(blimp, name, new_symbol->length+1, (char **)&new_symbol->name));

    *symbol = new_symbol;
    blimp->symbols.symbols[blimp->symbols.size++] = *symbol;
    return BLIMP_OK;
}
