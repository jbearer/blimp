#ifndef BLIMP_BLIMP_H
#define BLIMP_BLIMP_H

#include "../blimp.h"
#include "internal/common.h"
#include "internal/object.h"
#include "internal/stack.h"
#include "internal/symbol.h"
#include "internal/vtable.h"

#define ERR_MSG_LEN 100

struct Blimp {
    BlimpOptions options;
    SymbolTable symbols;
    ObjectPool objects;
    VTable vtable;
    CallStack stack;
    Object *global;

    const Symbol *this_symbol;
    const Symbol *that_symbol;

    struct BlimpErrorInfo {
        BlimpErrorCode code;
        StackTrace *trace;
        SourceRange range;
        bool has_range;
        char message[ERR_MSG_LEN];
    } last_error;
};

#endif
