#ifndef BLIMP_BLIMP_H
#define BLIMP_BLIMP_H

#include "../blimp.h"

// Abbreviations for common types.
typedef BlimpStatus Status;
typedef BlimpSourceLoc SourceLoc;
typedef BlimpSourceRange SourceRange;
typedef BlimpSymbol Symbol;
typedef BlimpStream Stream;
typedef BlimpExpr Expr;

#define ERR_MSG_LEN 100

// A SymbolTable is effectively a map from strings to the unique symbols
// representing them. Currently, this map is implemented as a vector with linear
// search, but it should soon be replaced by a hash table.
typedef struct {
    const Symbol **symbols;
    size_t size;
    size_t capacity;
} SymbolTable;

struct Blimp {
    SymbolTable symbols;

    struct BlimpErrorInfo {
        BlimpErrorCode code;
        SourceRange range;
        bool has_range;
        char message[ERR_MSG_LEN];
    } last_error;
};

#endif
