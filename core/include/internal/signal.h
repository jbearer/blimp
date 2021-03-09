#ifndef BLIMP_INTERRUPT_H
#define BLIMP_INTERRUPT_H

#include <signal.h>
#include <stdint.h>

#include "common.h"

typedef struct {
    BlimpSignalCallback callback;
    void *arg;
} SignalHandler;

typedef struct {
    Blimp *blimp;

    volatile sig_atomic_t status;
        // The status word indicates whether signals are enabled or not, and, if
        // enabled which signals are pending for delivery.
        //
        // The lowest bit of this word is 1 if and only if signals are enabled.
        // The other bits represent a set of pending signals, where for
        // `0 <= i <= BLIMP_MAX_SIGNAL`, signal `i` is pending if bit `i + 1` is
        // set.
        //
        // This is the only piece of state which is accessed by
        // Blimp_RaiseSignal, which can be called from signal handlers and from
        // other threads, hence the `volatile` qualifier. All accesses to this
        // state must be atomic.

    SignalHandler handlers[BLIMP_MAX_SIGNAL + 1];
} Signals;

PRIVATE void InitSignals(Blimp *blimp, Signals *signals);

/**
 * \brief Allow signals to be pended when Blimp_RaiseSignal is called.
 */
PRIVATE void EnableSignals(Signals *signals);

/**
 * \brief
 *      Disallow the raising of new signals, and execute any signals which are
 *      pended.
 *
 * The execution of pending signals is atomic with respect to the disabling of
 * signals, meaning no signals can be lost: any signals which were pended before
 * signals are atomically disabled will be executed, and any calls to
 * Blimp_RaiseSignal() after signals are atomically disabled will return
 * `BLIMP_SIGNAL_DISABLED`.
 *
 * Since this function executes signal handlers, it must be called when the
 * interpreter is in a consistent state (same as HandleSignals()).
 *
 * The return value is `BLIMP_OK` if all pending signals handlers returned
 * `BLIMP_OK`, or else the error code returned by the first handler to return an
 * error.
 */
PRIVATE Status DisableSignals(Signals *signals);

/**
 * \brief Disallow the raising of new signals, and forget pended signals.
 */
PRIVATE void ClearAndDisableSignals(Signals *signals);

/**
 * \brief Execute pended signals.
 *
 * Since this function executes signal handlers, it must be called when the
 * interpreter is in a consistent state (same as HandleSignals()).
 */
PRIVATE Status InternalHandleSignals(Signals *signals, const Instruction *ip);
static inline Status HandleSignals(Signals *signals, const Instruction *ip)
{
    // In the common case, there are no signals pending. We can do this check
    // early to avoid doing an expensive atomic exchange, which we need to do if
    // there are pending signals.
    //
    // Right-shift `status` by 1 bit to get the set of pending signals (since
    // the lowest bit is the status bit).
    if (!(signals->status >> 1)) {
        return BLIMP_OK;
    }

    return InternalHandleSignals(signals, ip);
}

#endif
