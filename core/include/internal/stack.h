#ifndef BLIMP_STACK_H
#define BLIMP_STACK_H

#include <stdint.h>

#include "internal/instruction.h"
#include "internal/object.h"

typedef struct StackFrame {
    bool has_range;
    SourceRange range;
    ScopedObject *scope;
    Object *message;
    const Instruction *return_address;
    Bytecode *executing;
    bool use_result;
} StackFrame;

struct BlimpStackTrace {
    StackFrame *frames;
        // Array of stack frames. The top of the stack is `frames[0]`, and the
        // stack grows towards higher indices. If this stack has been saved,
        // every frame in `frames` represents a valid part of the stack. If this
        // stack has not yet been saved, there may be uninitalized frames past
        // `next`.
    StackFrame *end;
        // A pointer to the end of the last frame allocated in `frames`.
    StackFrame *next;
        // For saved traces, this points to the end of the current frame. For
        // unsaved traces, this points to the frame which will be allocated by
        // the next call to Stack_Push().
};

typedef struct {
    StackTrace trace;
    uint8_t *start;
    uint8_t *end;
    bool grows_down;
} CallStack;

PRIVATE Status Stack_Init(Blimp *blimp, CallStack *stack);
PRIVATE void Stack_Destroy(Blimp *blimp, CallStack *stack);
PRIVATE Status Stack_Push(
    Blimp *blimp, CallStack *stack, StackFrame *frame, size_t space_request);
PRIVATE void Stack_Pop(Blimp *blimp, CallStack *stack);
PRIVATE const StackFrame *Stack_CurrentFrame(const CallStack *stack);
PRIVATE const StackFrame *Stack_BottomFrame(const CallStack *stack);
PRIVATE const StackFrame *StackTrace_CurrentFrame(const StackTrace *trace);

static inline bool Stack_Empty(CallStack *stack)
{
    return stack->trace.next == stack->trace.frames;
}

#endif
