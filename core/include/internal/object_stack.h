////////////////////////////////////////////////////////////////////////////////
// ObjectStack
//
// It's a stack of objects. What'd you expect?
//

#ifndef BLIMP_OBJECT_STACK_H
#define BLIMP_OBJECT_STACK_H

#include "internal/common.h"
#include "internal/result_stack.h"

SPECIALIZE_RESULT_STACK(ObjectStack, Object);

#endif
