#include <assert.h>

#include "internal/blimp.h"
#include "internal/expr.h"
#include "internal/error.h"
#include "internal/symbol.h"

static size_t StringHash(const char **str)
{
    // FNV1a hash function.

    static const size_t offset = 14695981039346656037ull;
    static const size_t prime  = 1099511628211ull;

    size_t hash = offset;
    for (const char *c = *str; *c; ++c) {
        hash = (hash ^ *c) * prime;
    }
    return hash;
}

static bool StringEq(const char **str1, const char **str2)
{
    return strcmp(*str1, *str2) == 0;
}

PRIVATE size_t SymbolHash(const Symbol **symbol)
{
    return (*symbol)->hash;
}

bool SymbolEq(const Symbol **sym1, const Symbol **sym2)
{
    return *sym1 == *sym2;
}

Status SymbolTable_Init(Blimp *blimp, SymbolTable *symbols)
{
    return HashMap_Init(
        blimp, symbols, sizeof(char *), sizeof(Symbol *),
        (EqFunc)StringEq, (HashFunc)StringHash, NULL);
}

void SymbolTable_Destroy(SymbolTable *symbols)
{
    Blimp *blimp = HashMap_GetBlimp(symbols);

    for (HashMapEntry *entry = HashMap_Begin(symbols);
         entry != HashMap_End(symbols);
         entry = HashMap_Next(symbols, entry))
    {
        Free(blimp, (char **)HashMap_GetKey(symbols, entry));
        Free(blimp, (Symbol **)HashMap_GetValue(symbols, entry));
    }

    HashMap_Destroy(symbols);
}

Status SymbolTable_GetSymbol(
    SymbolTable *symbols, const char *name, const Symbol **symbol)
{
    Blimp *blimp = HashMap_GetBlimp(symbols);

    HashMapEntry *entry;
    bool created;
    TRY(HashMap_Emplace(symbols, &name, &entry, &created));

    char **key;
    Symbol **value;
    size_t hash;
    HashMap_GetEntry(symbols, entry, (void **)&key, (void **)&value, &hash);

    if (!created) {
        // The entry we are using was already in the map, so it's fully valid,
        // and there's nothing else for us to do.
        *symbol = *value;
        return BLIMP_OK;
    }

    // Otherwise, we created and inserted a new entry into the map. Now we need
    // to initialie it. It's key is currently the same pointer as `name`:
    assert(*key == name);
    // In order to take ownership of that string, we need to duplicate it into
    // our own memory:
    size_t len = strlen(name);
    Status ret;
    if ((ret = Strndup(blimp, name, len+1, key)) != BLIMP_OK) {
        HashMap_AbortEmplace(symbols, entry);
        return ret;
    }
    // The value of the entry is completely uninitialized. We need to create a
    // new symbol for it:
    Symbol *new_symbol;
    if ((ret = Malloc(blimp, sizeof(Symbol), &new_symbol)) != BLIMP_OK) {
        HashMap_AbortEmplace(symbols, entry);
        return ret;
    }
    Object_Init((Object *)new_symbol, blimp, OBJ_SYMBOL);
    new_symbol->length = len;
    new_symbol->name   = *key;
    new_symbol->hash   = hash;
    *value  = new_symbol;
    *symbol = new_symbol;

    HashMap_CommitEmplace(symbols, entry);
    return BLIMP_OK;
}

const char *BlimpSymbol_GetName(const Symbol *sym)
{
    return sym->name;
}

size_t BlimpSymbol_Hash(const Symbol *sym)
{
    return sym->hash;
}
