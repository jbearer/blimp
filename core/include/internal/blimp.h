#ifndef BLIMP_BLIMP_H
#define BLIMP_BLIMP_H

#include "../blimp.h"
#include "internal/common.h"
#include "internal/object.h"
#include "internal/stack.h"
#include "internal/symbol.h"

#define ERR_MSG_LEN 100

struct Blimp {
    BlimpOptions options;
    SymbolTable symbols;
    ObjectPool objects;
    CallStack stack;
    Object *global;

    struct BlimpErrorInfo {
        BlimpErrorCode code;
        StackTrace *trace;
        SourceRange range;
        bool has_range;
        char message[ERR_MSG_LEN];
    } last_error;
};

#endif
