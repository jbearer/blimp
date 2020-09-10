#ifndef BLIMP_EVAL_H
#define BLIMP_EVAL_H

#include "internal/common.h"
#include "internal/object.h"

PRIVATE Status EvalBytecode(
    Blimp *blimp,
    ScopedObject *scope,
    const Bytecode *code,
    Object **result);

PRIVATE Status Send(
    Blimp *blimp,
    ScopedObject *scope,
    Object *receiver,
    Object *message,
    Object **result);

#endif
