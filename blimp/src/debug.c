#include "debug.h"
#include "hash_map.h"
#include "readline.h"

#define DEBUGGER_SIGNAL BLIMP_MAX_SIGNAL

static bool PointerEq(const void **p1, const void **p2, void *arg)
{
    (void)arg;
    return *p1 == *p2;
}

static size_t PointerHash(const void **p, void *arg)
{
    (void)arg;

    size_t hash = HASH_SEED;
    Hash_AddPointer(&hash, *p);
    return hash;
}

typedef struct {
    size_t stack_depth;
        // The target stack depth. The breakpoint will not trigger when hit
        // unless the depth of the interpreter stack matches `stack_depth`, or
        // `stack_depth` is `BREAKPOINT_STACK_DEPTH_ANY`. This is used
        // internally to step through a specific instance of a procedure, for
        // example by the Debugger_StepI() and Debugger_Finish() commands.
    size_t count;
        // The (non-zero) remaining number of times this breakpoint must be hit
        // before it triggers.
    size_t reset_count;
        // The (non-zero) maximum number of consecutive times this breakpoint
        // can be hit before triggering. Once a breakpoint triggers, its `count`
        // is restored to its `reset_count`.
    Expr *condition;
        // Condition to determine if a breakpoint should trigger. If non-NULL,
        // this expression is evaluated in the current scope whenever the
        // breakpoint is hit, and the breakpoint only triggers if the result is
        // the symbol `true`.
    bool temporary;
        // If set, the breakpoint will be deleted after it triggers.
} Breakpoint;

#define BREAKPOINT_STACK_DEPTH_ANY ((size_t)-1)

static inline Status Debugger_SetBreakpoint(
    Debugger *db, const Instruction *addr, Breakpoint *bp)
{
    assert(bp->count > 0);
    bp->reset_count = bp->count;
    return HashMap_Update(&db->breakpoints, &addr, bp);
}

// Instruction callback used when we are attached to non-user code (for example
// when we are executing commands inside the debugger REPL).
static Status Debugger_IgnoreInstruction(
    Blimp *blimp, size_t signum, const Instruction *ip, void *arg)
{
    (void)blimp;
    (void)signum;
    (void)ip;
    (void)arg;
    return BLIMP_OK;
}

// Instruction callback used when we are attached to user code. The purpose of
// this function is to quickly decide if we should stop at this instruction and,
// if so, drop into the REPL.
static Status Debugger_HandleInstruction(
    Blimp *blimp, size_t signum, const Instruction *ip, void *arg)
{
    Blimp_PendSignal(blimp, signum);
        // Re-raise the signal that caused this handler to run so that it runs
        // again before the next instruction.

    if (ip == NULL) {
        return BLIMP_OK;
    }

    Debugger *db = (Debugger *)arg;
    db->ip = ip;

    if (db->stop_at_next_instruction) {
        // If we were told to stop at the next instruction (for example, we're
        // doing a stepi) stop and drop into the REPL.
        db->stop_at_next_instruction = false;
        return Debugger_Repl(db);
    }

    // Check if there is a breakpoing at the current PC. If not, return to user
    // code. If there is, execute the breakpoint conditions and figure out if we
    // should halt.
    Breakpoint *bp = HashMap_Find(&db->breakpoints, &ip);
    if (bp == NULL) {
        return BLIMP_OK;
    }

    if (bp->stack_depth != BREAKPOINT_STACK_DEPTH_ANY &&
        bp->stack_depth != Blimp_StackDepth(blimp))
    {
        // If the breakpoint has a target stack depth which does not match the
        // current stack depth, it should not trigger.
        return BLIMP_OK;
    }

    // Decrement the breakpoint's counter.
    assert(bp->count > 0);
    if (--bp->count > 0) {
        // If the counter has not yet reached 0, the breakpoint should not
        // trigger.
        return BLIMP_OK;
    }
    bp->count = bp->reset_count;
        // If the count has reached 0, reset it and then proceed with triggering
        // the breakpoint.

    if (bp->condition != NULL) {
        // If the breakpoint has a condition, evaluate it and compare the result
        // to `true`.
        const Symbol *true_sym;
        TRY(Blimp_GetSymbol(blimp, "true", &true_sym));

        const Symbol *cond_result;
        TRY(Blimp_EvalSymbol(
            blimp,
            bp->condition,
            Blimp_CurrentScope(blimp),
            &cond_result
        ));

        if (cond_result != true_sym) {
            // If the condition is not met, abort triggering the breakpoint.
            return BLIMP_OK;
        }
    }

    if (bp->temporary) {
        // If we have successfully triggered a temporary breakpoint, delete it.
        HashMap_Remove(&db->breakpoints, &ip, NULL);
    }

    return Debugger_Repl(db);
        // If we get here, a breakpoint has triggered, so drop into the REPL.
}

static void Debugger_DropIn(Debugger *db)
{
    // Tell the interpreter to call Debugger_HandleInstruction() as soon as it
    // starts executing. Debugger_HandleInstruction() will re-raise the signal
    // each time it is called so that it gets called again after every
    // instruction.
    Blimp_HandleSignal(
        db->blimp, DEBUGGER_SIGNAL, Debugger_HandleInstruction, db);
    Blimp_PendSignal(db->blimp, DEBUGGER_SIGNAL);
}

static void Debugger_DropOut(Debugger *db)
{
    Blimp_HandleSignal(
        db->blimp, DEBUGGER_SIGNAL, Debugger_IgnoreInstruction, db);
}

void Debugger_Init(Debugger *db)
{
    db->blimp = NULL;
        // Set `blimp` to `NULL` to indicate we are not attached to any bl:mp.
        // The rest of the initialization will be done when we attach.
}

Status Debugger_Attach(Debugger *db, Blimp *blimp)
{
    if (db->blimp != NULL) {
        return Blimp_ErrorMsg(
            blimp, BLIMP_ERROR, "debugger is already attached");
    }

    db->blimp = blimp;
    db->ip = NULL;
    db->stop_at_next_instruction = true;
        // Set up to drop into the REPL before we actually execute any code...
    db->resume = false;
        // ...and stay in the REPL until we get a "resume" command from the
        // user.

    TRY(HashMap_Init(
        blimp,
        &db->breakpoints,
        sizeof(Instruction *),
        sizeof(Breakpoint),
        (EqFunc)PointerEq,
        (HashFunc)PointerHash,
        NULL
    ));

    Debugger_DropIn(db);
    return BLIMP_OK;
}

void Debugger_Detach(Debugger *db)
{
    if (db->blimp != NULL) {
        Debugger_DropOut(db);
        HashMap_Destroy(&db->breakpoints);
        db->blimp = NULL;
    }
}

Blimp *Debugger_GetAttachedBlimp(const Debugger *db)
{
    return db->blimp;
}

Status Debugger_Break(
    Debugger *db,
    const Instruction *bp,
    size_t count,
    Expr *condition,
    bool temporary)
{
    return Debugger_SetBreakpoint(db, bp, &(Breakpoint) {
        .count = count,
        .condition = condition,
        .temporary = temporary,
        .stack_depth = BREAKPOINT_STACK_DEPTH_ANY,
    });
}

Status Debugger_StepI(Debugger *db)
{
    db->stop_at_next_instruction = true;
    return Debugger_Continue(db);
}

Status Debugger_NextI(Debugger *db)
{
    const Instruction *next = BlimpInstruction_Next(db->ip);
    if (next != NULL) {
        // If there is an instruction after this one in the current procedure,
        // set a temporary breakpoint there and require that the stack depth is
        // the same as the current stack depth (otherwise we might hit the same
        // instruction in a recursive procedure call).
        TRY(Debugger_SetBreakpoint(db, next, &(Breakpoint) {
            .count = 1,
            .condition = NULL,
            .temporary = true,
            .stack_depth = BlimpStackTrace_Size(db->trace),
        }));
        return Debugger_Continue(db);
            // Run to the temporary breakpoint.
    } else {
        // If there is no next instruction (that is, we are at the last
        // instruction in the procedure) then just finish the procedure.
        return Debugger_Finish(db);
    }
}

Status Debugger_Finish(Debugger *db)
{
    const Instruction *ret = BlimpStackTrace_GetReturnAddress(db->trace);

    if (ret != NULL) {
        // If there is a return address, set a breakpoint there and require that
        // the stack depth decreases by 1 (otherwise we might hit the same
        // instruction in a nested procedure call).
        TRY(Debugger_SetBreakpoint(db, ret, &(Breakpoint) {
            .count = 1,
            .condition = NULL,
            .temporary = true,
            .stack_depth = BlimpStackTrace_Size(db->trace) - 1,
        }));
    } else {
        // A NULL return address indicates that this is the top-level procedure,
        // so running until it completes just means finishing execution. Set no
        // breakpoints and resume execution.
    }

    return Debugger_Continue(db);
}

Status Debugger_Continue(Debugger *db)
{
    db->resume = true;
    return BLIMP_OK;
}

Status Debugger_Bt(Debugger *db)
{
    BlimpStackTrace_Print(stdout, db->trace, 0);
    return BLIMP_OK;
}

Status Debugger_List(Debugger *db)
{
    const BlimpBytecode *proc = BlimpStackTrace_GetProcedure(db->trace);
    BlimpBytecode_PrintWithIP(stdout, proc, db->ip, false);
    return BLIMP_OK;
}

Status Debugger_Repl(Debugger *db)
{
    BlimpInstruction_PrintCurrent(stdout, db->ip);

    // Get a stack trace to work with during this REPL session.
    TRY(Blimp_SaveStackTrace(db->blimp, &db->trace));

    // Detach while we execute debugger commands.
    Debugger_DropOut(db);

    // Read and execute expressions from the command line until either
    //  1. we get EOF, which we interpret as quitting the debugger
    //  2. the evaluation of an expression executes a debugger command that sets
    //     `db->resume` (such as `?db continue`), in which case we return to
    //     the user's program
    Expr *expr;
    while ((expr = Readline_ReadExpr(db->blimp, "bl:db> ", true)) != NULL) {
        if (Blimp_Eval(db->blimp, expr, Blimp_CurrentScope(db->blimp), NULL)
                != BLIMP_OK)
        {
            Blimp_FreeExpr(expr);
            Blimp_DumpLastError(db->blimp, stdout);
            continue;
                // If there is an error in the expression, we just continue
                // reading more commands.
        }
        Blimp_FreeExpr(expr);

        if (db->resume) {
            db->resume = false;
                // Clear the `resume` flag so that we will stay in the next REPL
                // loop until we get another resume command.
            Blimp_FreeStackTrace(db->blimp, db->trace);

            // Reattach as we return to user code, unless the user gave the
            // `detach` commmand.
            if (db->blimp != NULL) {
                Debugger_DropIn(db);
            }
            return BLIMP_OK;
        }
    }

    Blimp_FreeStackTrace(db->blimp, db->trace);
    return Blimp_Error(db->blimp, BLIMP_INTERRUPTED);
        // If the user sends EOF, quit the current computation.
}
