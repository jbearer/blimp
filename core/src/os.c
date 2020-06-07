#ifdef __GNUC__
# define _GNU_SOURCE
#endif

#include <alloca.h>
#include <pthread.h>

#include "internal/error.h"
#include "internal/os.h"

static __attribute__((noinline)) Status StackGrowsDown(
    Blimp *blimp, bool *grows_down)
{
    // Allocate two pointers on the stack. The second address will be offset
    // from the first address in the direction of stack growth.
    uint8_t *p1 = alloca(sizeof(uint8_t));
    uint8_t *p2 = alloca(sizeof(uint8_t));

    assert(p1 != p2);

    if (p1 == NULL || p2 == NULL) {
        // We're already out of stack space, before we can even observe the
        // stack growing. This case is extremely unlikely, but we still want to
        // handle it gracefully.
        //
        // Note that `alloca` is not guaranteed to be NULL if we're out of stack
        // space; it's technically undefined behavior to overflow the stack. But
        // it _may_ return NULL, and if we don't have this check, we may get the
        // wrong answer for `grows_down`, since the address we'd be checking
        // (NULL) isn't really an address on the stack.
        //
        // Any other thing `alloca` does on stack overflow is likely to either
        // kill the program or give a reasonable result (for example, return an
        // address past the end of the stack, which would still give us the
        // right idea about which way the stack is growing).
        return Error(blimp, BLIMP_STACK_OVERFLOW);
    }

    *grows_down = p2 < p1;
    return BLIMP_OK;
}

Status OS_GetStackLimits(
    Blimp *blimp, uint8_t **lo, uint8_t **hi, bool *grows_down)
{
    TRY(StackGrowsDown(blimp, grows_down));

#ifdef __GNUC__

    // Get the thread attributes.
    pthread_attr_t attr;
    int err = pthread_getattr_np(pthread_self(), &attr);
    if (err) {
        return ErrorMsg(blimp, BLIMP_ERROR,
            "unable to get pthread attributes: %s", strerror(err));
    }

    // Extract the stack start and size from the attributes.
    void *stack_start;
    size_t stack_size;
    if ((err = pthread_attr_getstack(&attr, &stack_start, &stack_size)) != 0) {
        return ErrorMsg(blimp, BLIMP_ERROR,
            "unable to get pthread stack: %s", strerror(err));
    }

    pthread_attr_destroy(&attr);

    // Figure out of `stack_start` is the lowest or highest address in the
    // stack, and compute the other limit based on `stack_size`.
    if (*grows_down) {
        *hi = (uint8_t *)stack_start;
        *lo = (uint8_t *)stack_start - stack_size;
    } else {
        *lo = (uint8_t *)stack_start;
        *hi = (uint8_t *)stack_start + stack_size;
    }

    return BLIMP_OK;

#else
    (void)lo;
    (void)hi;
    (void)grows_down;

    return ErrorMsg(
        blimp,
        BLIMP_NOT_SUPPORTED,
        "OS_GetStackLimits is not supported on this system");
#endif
}
