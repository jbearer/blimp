#include <alloca.h>
#include <pthread.h>

#include "internal/blimp.h"
#include "internal/error.h"
#include "internal/os.h"
#include "internal/stack.h"

static __attribute__((noinline)) bool HasSpace(
    const CallStack *stack, size_t space_request)
{
    uint8_t *stack_var = alloca(sizeof(uint8_t));

    if (stack_var == NULL) {
        return false;
    }

    if (stack->end == NULL) {
        // If we weren't able to figure out where the end of the stack is, we
        // optimistically assume we have space and let the OS deal with us if we
        // are wrong and try to access past the end of the stack.
        return true;
    }

    if (stack->grows_down) {
        if (stack_var - space_request < stack->end) {
            return false;
        }
    } else {
        if (stack_var + space_request > stack->end) {
            return false;
        }
    }

    return true;
}

Status Stack_Init(Blimp *blimp, CallStack *stack)
{
    // Figure out the end address for the C runtime stack by querying the
    // underlying thread. Storing this information in the `stack` data structure
    // means we are making the assumption that this is the only thread which
    // will ever interact with this stack. Since the bl:mp interpreter is
    // currently not threadsafe, this is not an unreasonable assumption to make.
    // However, it means that users might see strange behavior if they use the
    // interpreter with multiple threads, even if they do their own locking. It
    // would not be too difficult to simply store this information in thread-
    // local storage (for example, using the pthread_key API) if it ever becomes
    // desirable to support multiple call stacks per interpreter.
    if (OS_GetStackLimits(
            blimp, &stack->start, &stack->end, &stack->grows_down) != BLIMP_OK)
    {
        // If we couldn't get the stack bounds (perhaps it is not implemented
        // for this OS/architecture?) proceed anyways. We just won't have
        // graceful error handling for stack overflows.
        stack->start = NULL;
        stack->end   = NULL;
    }

    // Initialize the trace.
    TRY(Malloc(
        blimp,
        blimp->options.recursion_limit*sizeof(StackFrame),
        &stack->trace.frames));
    stack->trace.end  = stack->trace.frames + blimp->options.recursion_limit;
    stack->trace.next = stack->trace.frames;

    return BLIMP_OK;
}

void Stack_Destroy(Blimp *blimp, CallStack *stack) {
    Free(blimp, &stack->trace.frames);
}

Status Stack_Push(
    Blimp *blimp, CallStack *stack, StackFrame *frame, size_t space_request)
{
    if (stack->trace.next >= stack->trace.end ||
            // Check for space in the stack trace.
        !HasSpace(stack, space_request)
            // Check for space on the actual call stack.
    ) {
        return RuntimeError(blimp, BLIMP_STACK_OVERFLOW);
    }

    *stack->trace.next = *frame;
    ++stack->trace.next;
    return BLIMP_OK;
}

void Stack_Pop(Blimp *blimp, CallStack *stack)
{
    (void)blimp;

    assert(stack->trace.next > stack->trace.frames);
    --stack->trace.next;
}

const StackFrame *Stack_CurrentFrame(const CallStack *stack)
{
    return StackTrace_CurrentFrame(&stack->trace);
}

Status Blimp_SaveStackTrace(Blimp *blimp, StackTrace **trace)
{
    return Blimp_CopyStackTrace(blimp, &blimp->stack.trace, trace);
}

Status Blimp_CopyStackTrace(
    Blimp *blimp, const StackTrace *from, StackTrace **to)
{
    *to = NULL;

    size_t size = from->next - from->frames;
    TRY(Malloc(blimp, sizeof(StackTrace) + size*sizeof(StackFrame), to));

    (*to)->frames = (StackFrame *)((uint8_t *)*to + sizeof(StackTrace));
    memcpy((*to)->frames, from->frames, size*sizeof(StackFrame));
        // Copy all the valid frames.
    (*to)->next = (*to)->frames + (from->next - from->frames);
        // Set up `next` at an offset matching its offsets in `from`.
    (*to)->end = (*to)->next;
        // We only allocated exactly enough space for the frames that currently
        // exist, so `next` and `end` are the same.

    return BLIMP_OK;
}

void Blimp_FreeStackTrace(Blimp *blimp, StackTrace *trace)
{
    Free(blimp, &trace);
}

bool BlimpStackTrace_Up(StackTrace *trace)
{
    if (trace->next > trace->frames) {
        --trace->next;
    }

    return trace->next > trace->frames;
}

bool BlimpStackTrace_Down(StackTrace *trace)
{
    if (trace->next <= trace->end) {
        ++trace->next;
    }

    return trace->next <= trace->end;
}

void BlimpStackTrace_GetRange(const StackTrace *trace, SourceRange *range)
{
    *range = StackTrace_CurrentFrame(trace)->range;
}

void BlimpStackTrace_Print(FILE *file, const StackTrace *trace, size_t limit)
{
    for (StackFrame *frame = trace->frames; frame < trace->end; ++frame) {
        if (limit &&
                // If the number of frames to show is limited...
            (size_t)(frame - trace->frames) > limit &&
                // ...and we're more than `limit` frames from the start...
            (size_t)(trace->end - frame) > limit
                // ...and we're more than `limit` frames from the end...
        ) {
            // ...then this frame is elided.
            if ((size_t)(frame - trace->frames) == limit + 1) {
                // If this is the first elided frame, print and ellipsis.
                fprintf(file, "...\n");
            }

            // Skip the frame.
            continue;
        }

        if (frame->has_range) {
            fprintf(file, "#%zu at ", frame - trace->frames);
            PrintSourceRange(file, &frame->range);
        }
    }
}

const StackFrame *StackTrace_CurrentFrame(const StackTrace *trace)
{
    if (trace->next > trace->frames && trace->next <= trace->end) {
        return trace->next - 1;
    } else {
        return NULL;
    }
}
