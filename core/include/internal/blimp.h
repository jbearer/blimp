#ifndef BLIMP_BLIMP_H
#define BLIMP_BLIMP_H

#include "common.h"

#include "../blimp.h"
#include "internal/object.h"
#include "internal/object_stack.h"
#include "internal/optimizer.h"
#include "internal/parse.h"
#include "internal/signal.h"
#include "internal/stack.h"
#include "internal/symbol.h"

#define ERR_MSG_LEN 100

struct Blimp {
    BlimpOptions options;
    SymbolTable symbols;
    ObjectPool objects;
    CallStack stack;
    ObjectStack result_stack;
    GlobalObject *global;
    Signals signals;
    Optimizer optimizer;
    TokenTrie tokens;
    Grammar grammar;
    size_t counter;

    struct BlimpErrorInfo {
        BlimpErrorCode code;
        StackTrace *trace;
        SourceRange range;
        bool has_range;
        char message[ERR_MSG_LEN];
    } last_error;
};

#endif
