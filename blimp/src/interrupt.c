#include <signal.h>
#include <stdlib.h>

#include "common.h"
#include "interrupt.h"

typedef struct InterruptContext {
    InterruptHandler handler;
    void *arg;
    struct InterruptContext *saved;
} InterruptContext;

static InterruptContext *volatile context = NULL;

static void DispatchSignal(int signum)
{
    (void)signum;
    context->handler(context->arg);
}

bool PushInterruptHandler(InterruptHandler handler, void *arg)
{
    signal(SIGINT, SIG_IGN);

    // Create a new interrupt context.
    InterruptContext *ctx = malloc(sizeof(InterruptContext));
    if (ctx == NULL) {
        return false;
    }
    ctx->handler = handler;
    ctx->arg = arg;

    // Push the new context onto the stack.
    ctx->saved = context;
    context = ctx;

    if (handler) {
        signal(SIGINT, DispatchSignal);
    } else {
        // Disable interrupts if the handler isn't going to do anything.
        signal(SIGINT, SIG_IGN);
    }

    return true;
}

void PopInterruptHandler(void)
{
    assert(context != NULL);

    InterruptContext *popped = context;
    context = popped->saved;
    free(popped);
}
