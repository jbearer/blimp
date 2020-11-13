#ifndef BLIMP_SYSTEM_H
#define BLIMP_SYSTEM_H

#include <blimp/module.h>

BlimpStatus Function(
    Blimp *blimp, const char *name, BlimpMethod method, void *arg);
BlimpStatus VoidReturn(Blimp *blimp, BlimpObject **result);

#endif
