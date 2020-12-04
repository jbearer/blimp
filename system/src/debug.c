#include <signal.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <unistd.h>

#include "system.h"

static bool IsDebuggerAttached(void)
{
    pid_t pid = fork();
    if (pid) {
        int status;
        waitpid(pid, &status, 0);
        return WEXITSTATUS(status);
    } else {
        if (ptrace(PTRACE_ATTACH, getppid(), 0, 0) == 0) {
            ptrace(PTRACE_DETACH, getppid(), 0, (void *)SIGCONT);
            exit(0);
        } else {
            // ptrace will fail if we are already being traced by a debugger.
            exit(1);
        }
    }
}

static BlimpStatus DebugBreak(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)scope;
    (void)receiver;
    (void)message;

    if (IsDebuggerAttached()) {
        raise(SIGTRAP);
    }
    return VoidReturn(blimp, result);
}

static BlimpStatus InitSystemDebug(
    Blimp *blimp, BlimpObject *context, BlimpObject **result)
{
    (void)context;

    if (Function(blimp, "__debug_break", DebugBreak, NULL) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return VoidReturn(blimp, result);
}

BLIMP_MODULE(InitSystemDebug);
