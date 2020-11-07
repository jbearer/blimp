////////////////////////////////////////////////////////////////////////////////
// ResultStack
//
// The stack which holds results of bl:mp instructions. Elements are pointer-
// sized.
//

#ifndef BLIMP_RESULT_STACK_H
#define BLIMP_RESULT_STACK_H

#include "../blimp.h"
#include "internal/common.h"

typedef struct {
    size_t capacity;
    void **end;
        // We store the end, not the start, because overflows are runtime
        // checked while underflows are not. This is because a user can cause an
        // overflow in the bl:mp interpreter result stack by writing a
        // sufficiently recursive program. But a user cannot cause an underflow,
        // because, assuming the compiler is correct, we will only every pop
        // objects which the user has caused to be pushed.
        //
        // The start of the stack can still be recovered using `end - capacity`.
    void **top;
        // Pointer to the next free object in the stack. The top valid object is
        // located at `top - 1`.
} ResultStack;

static inline Status ResultStack_Init(
    Blimp *blimp, ResultStack *stack, size_t size)
{
    stack->capacity = size;
    TRY(Malloc(blimp, stack->capacity*sizeof(Object *), &stack->top));
    stack->end = stack->top + stack->capacity;

    return BLIMP_OK;
}

static inline void ResultStack_Destroy(Blimp *blimp, ResultStack *stack)
{
    void **start = stack->end - stack->capacity;
    Free(blimp, &start);
}

static inline Status ResultStack_Push(
    Blimp *blimp, ResultStack *stack, void *entry)
{
    if (stack->top >= stack->end) {
        return Error(blimp, BLIMP_STACK_OVERFLOW);
    }

    *stack->top++ = entry;
    return BLIMP_OK;
}

static inline void *ResultStack_Pop(Blimp *blimp, ResultStack *stack)
{
    (void)blimp;

    assert(stack->top > (stack->end - stack->capacity));
    return *--stack->top;
}

static inline size_t ResultStack_Size(const ResultStack *stack)
{
    return stack->capacity - (stack->end - stack->top);
}

static inline bool ResultStack_Empty(const ResultStack *stack)
{
    return ResultStack_Size(stack) == 0;
}

// Create a typesafe wrapper for a ResultStack containing elements of a
// specified type.
#define SPECIALIZE_RESULT_STACK(stack_type, entry_type) \
typedef struct { \
    ResultStack inner; \
} stack_type; \
\
static inline Status stack_type##_Init( \
    Blimp *blimp, stack_type *stack, size_t size) \
{ \
    return ResultStack_Init(blimp, &stack->inner, size); \
} \
\
static inline void stack_type##_Destroy(Blimp *blimp, stack_type *stack) \
{ \
    ResultStack_Destroy(blimp, &stack->inner); \
} \
\
static inline Status stack_type##_Push( \
    Blimp *blimp, stack_type *stack, entry_type *entry) \
{ \
    return ResultStack_Push(blimp, &stack->inner, entry); \
} \
\
static inline entry_type *stack_type##_Pop(Blimp *blimp, stack_type *stack) \
{ \
    return ResultStack_Pop(blimp, &stack->inner); \
} \
static inline size_t stack_type##_Size(const stack_type *stack) \
{ \
    return ResultStack_Size(&stack->inner); \
} \
static inline bool stack_type##_Empty(const stack_type *stack) \
{ \
    return ResultStack_Empty(&stack->inner); \
}

#endif
