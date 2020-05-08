#ifndef BLIMP_VTABLE_H
#define BLIMP_VTABLE_H

#include "internal/common.h"
#include "internal/hash_map.h"

typedef struct {
    HashMap methods;
    const Symbol *wildcard;
} VTable;

PRIVATE Status VTable_Init(Blimp *blimp, VTable *vtable);

PRIVATE void VTable_Destroy(VTable *vtable);

PRIVATE Status VTable_Bind(
    VTable *vtable,
    const Symbol *receiver,
    const Symbol *message,
    Method method,
    void *data);

PRIVATE Status VTable_Resolve(
    const VTable *vtable,
    const Symbol *receiver,
    const Symbol *message,
    Method *method,
    void **data);

#endif
