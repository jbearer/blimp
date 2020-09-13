#include <signal.h>

#include "interrupt.h"

static InterruptHandler sig_handler;
static void *sig_arg;

static void DispatchSignal(int signum)
{
    (void)signum;

    sig_handler(sig_arg);
}

void OnInterrupt(InterruptHandler handler, void *arg)
{
    signal(SIGINT, SIG_IGN);

    if (handler) {
        sig_handler = handler;
        sig_arg     = arg;
        signal(SIGINT, DispatchSignal);
    }
}

