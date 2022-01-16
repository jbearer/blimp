#include "internal/blimp.h"
#include "internal/signal.h"

static inline bool AtomicCompareExchange(
    volatile sig_atomic_t *addr, sig_atomic_t expected, sig_atomic_t desired)
{
    return __atomic_compare_exchange_n(
        addr, &expected, desired, false, __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE);
}

static inline sig_atomic_t AtomicExchange(
    volatile sig_atomic_t *addr, sig_atomic_t new_value)
{
    return __atomic_exchange_n(addr, new_value, __ATOMIC_ACQ_REL);
}

static inline size_t CountTrailingZeros(sig_atomic_t word)
{
    assert(word != 0);
    return __builtin_ctzl(word);
}

static inline sig_atomic_t SigMask(size_t signum)
{
    assert(signum + 1 < sizeof(sig_atomic_t)*CHAR_BIT);
    return ((size_t)1 << (signum + 1));
        // Offset by 1, since the first bit (the status bit) does not correspond
        // to any signal.
}

static inline size_t SigAtomicToUnsigned(sig_atomic_t x)
{
    if (sizeof(size_t) > sizeof(sig_atomic_t)) {
        return (size_t)x & (((size_t)1 << (sizeof(sig_atomic_t)*CHAR_BIT)) - 1);
            // Since we're casting `x` from a potentially signed type
            // (sig_atomic_t) to a larger unsigned type (size_t) we may get a
            // number of leading 1's if `x < 0`, due to sign extension. So we
            // mask off all of the bits that don't fall within
            // sizeof(sig_atomic_t).
    } else {
        assert(sizeof(size_t) == sizeof(sig_atomic_t));
            // size_t cannot be smaller than sig_atomic_t.
        return (size_t)x;
            // Both types are the same size so we don't have to worry about
            // sign-extended leading 1's.
    }
}

void InitSignals(Blimp *blimp, Signals *signals)
{
    signals->blimp = blimp;
    signals->status = 0;
        // Signals are initially disabled.
    memset(signals->handlers, 0, sizeof(signals->handlers));
        // Zero out the handlers. Any handler with a NULL `callback` will have
        // the default behavior if the corresponding signal is raised; it will
        // simply return the error `BLIMP_INTERRUPTED`.
}

Status InternalHandleSignals(Signals *signals, const Instruction *ip)
{
    // Atomically take the entire set of pended signals.
    size_t pending = SigAtomicToUnsigned(AtomicExchange(&signals->status, 1));
        // Store 1 into `status` so that the enabled bit is set, but the set of
        // pending signals is now empty.
    pending >>= 1;
        // Shift off the enabled bit; we only care about the set of pending
        // signals represented by the higher-order bits.

    while (pending) {
        size_t signum = CountTrailingZeros(pending);
        assert(signum <= BLIMP_MAX_SIGNAL);

        pending &= ~(1 << signum);
            // Clear the bit for the signal we're about to process.

        SignalHandler *handler = &signals->handlers[signum];
        Status status;
        if (handler->callback) {
            status = handler->callback(
                signals->blimp, signum, ip, handler->arg);
        } else {
            status = Error(signals->blimp, BLIMP_INTERRUPTED);
                // The default handler simply returns `BLIMP_INTERRUPTED`.
        }

        if (status != BLIMP_OK) {
            // If the handler returned an error, we must return the same error
            // to the caller. But first, add the set of pending signals which we
            // didn't process back to the pending set in `signals->status`.
            if (pending) {
                sig_atomic_t sig_status;
                do {
                    sig_status = signals->status;
                } while (!AtomicCompareExchange(
                    &signals->status, sig_status, sig_status | (pending << 1)));
            }

            return status;
        }
    }

    return BLIMP_OK;
}

void EnableSignals(Signals *signals)
{
    // Loop on CompareExchange until we successfully set the enabled bit in the
    // status word.
    sig_atomic_t status;
    do {
        status = signals->status;
    } while (!AtomicCompareExchange(&signals->status, status, status|1));
}

Status DisableSignals(Signals *signals)
{
    // Atomically take the entire set of pended signals.
    size_t pending = SigAtomicToUnsigned(AtomicExchange(&signals->status, 0));
        // Store 0 into `status` so that the enabled bit is cleared, and the set
        // of pending signals is now empty.
    pending >>= 1;
        // Shift off the enabled bit; we only care about the set of pending
        // signals represented by the higher-order bits.

    // Execute signals which were pended before we disabled signals.
    while (pending) {
        size_t signum = CountTrailingZeros(pending);
        assert(signum <= BLIMP_MAX_SIGNAL);

        pending &= ~(1 << signum);
            // Clear the bit for the signal we're about to process.

        SignalHandler *handler = &signals->handlers[signum];
        if (handler->callback) {
            if (handler->callback(
                    signals->blimp, signum, NULL, handler->arg) != BLIMP_OK)
            {
                return Reraise(signals->blimp);
                    // Unlike in HandleSignals(), we do not re-pend the
                    // unprocessed signals remaining in `pending`, because
                    // signals are now disabled. It would not make sense to add
                    // pending signals. Instead, those signals are simply lost.
            }
        } else {
            return Error(signals->blimp, BLIMP_INTERRUPTED);
        }
    }

    return BLIMP_OK;
}

void ClearAndDisableSignals(Signals *signals)
{
    signals->status = 0;
}

BlimpSignalError Blimp_HandleSignal(
    Blimp *blimp, size_t signum, BlimpSignalCallback callback, void *arg)
{
    if (signum > BLIMP_MAX_SIGNAL) {
        return BLIMP_SIGNAL_INVALID;
    }

    // This function is not meant to be thread-safe or signal-safe, and neither
    // are HandleSignals() or DisableSignals() (which are the other functions
    // that access `handlers`). Therefore, we don't have to worry that we are
    // initializing `handler->callback` and `handler->arg` non-atomically here.
    SignalHandler *handler = &blimp->signals.handlers[signum];
    handler->callback = callback;
    handler->arg = arg;

    return BLIMP_SIGNAL_OK;
}

BlimpSignalError Blimp_RaiseSignal(Blimp *blimp, size_t signum)
{
    if (signum > BLIMP_MAX_SIGNAL) {
        return BLIMP_SIGNAL_INVALID;
    }

    // As long as signals are enabled, try to set the bit corresponding to
    // `signum` in the status word using a compare-and-swap.
    size_t status;
    do {
        status = SigAtomicToUnsigned(blimp->signals.status);
        if (!(status & 1)) {
            return BLIMP_SIGNAL_DISABLED;
        }
    } while (!AtomicCompareExchange(
        &blimp->signals.status, status, status|SigMask(signum)));

    return BLIMP_SIGNAL_OK;
}

BlimpSignalError Blimp_PendSignal(Blimp *blimp, size_t signum)
{
    if (signum > BLIMP_MAX_SIGNAL) {
        return BLIMP_SIGNAL_INVALID;
    }

    // Loop on CompareExchange until we successfully set the bit corresponding
    // to `signum` in the status word.
    size_t status;
    do {
        status = SigAtomicToUnsigned(blimp->signals.status);
    } while (!AtomicCompareExchange(
        &blimp->signals.status, status, status|SigMask(signum)));

    return BLIMP_SIGNAL_OK;
}
