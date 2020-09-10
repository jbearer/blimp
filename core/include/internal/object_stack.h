////////////////////////////////////////////////////////////////////////////////
// ObjectStack
//
// It's a stack of objects. What'd you expect?
//

#ifndef BLIMP_OBJECT_STACK_H
#define BLIMP_OBJECT_STACK_H

#include "../blimp.h"
#include "internal/common.h"

typedef struct {
    size_t capacity;
    Object **end;
        // We store the end, not the start, because overflows are runtime
        // checked while underflows are not. This is because a user can cause an
        // overflow in the bl:mp interpreter result stack by writing a
        // sufficiently recursive program. But a user cannot cause an underflow,
        // because, assuming the compiler is correct, we will only every pop
        // objects which the user has caused to be pushed.
        //
        // The start of the stack can still be recovered using `end - capacity`.
    Object **top;
        // Pointer to the next free object in the stack. The top valid object is
        // located at `top - 1`.
} ObjectStack;

static inline Status ObjectStack_Init(
    Blimp *blimp, ObjectStack *stack, size_t size)
{
    stack->capacity = size;
    TRY(Malloc(blimp, stack->capacity*sizeof(Object *), &stack->top));
    stack->end = stack->top + stack->capacity;

    return BLIMP_OK;
}

static inline void ObjectStack_Destroy(Blimp *blimp, ObjectStack *stack)
{
    BlimpObject **start = stack->end - stack->capacity;
    Free(blimp, &start);
}

static inline Status ObjectStack_Push(Blimp *blimp, ObjectStack *stack, Object *obj)
{
    assert(obj != NULL);

    if (stack->top >= stack->end) {
        return Error(blimp, BLIMP_STACK_OVERFLOW);
    }

    *stack->top++ = obj;
    return BLIMP_OK;
}

static inline Object *ObjectStack_Pop(Blimp *blimp, ObjectStack *stack)
{
    (void)blimp;

    assert(stack->top > (stack->end - stack->capacity));
    return *--stack->top;
}

static inline size_t ObjectStack_Size(const ObjectStack *stack)
{
    return stack->capacity - (stack->end - stack->top);
}

#endif
