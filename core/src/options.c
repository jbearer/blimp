#include "internal/blimp.h"

const BlimpOptions DEFAULT_BLIMP_OPTIONS = {
    .recursion_limit        = 1000,
    .stack_trace_limit      = 5,
    .gc_batch_size          = (1ull<<20), // 1MB
    .gc_tracing             = true,
    .gc_batches_per_trace   = 1,
    .gc_refcount            = true,
    .gc_cycle_detection     = true,
    .gc_max_clump_size      = 0,
    .gc_heap_check          = false,
    .tail_call_elimination  = true,
    .constant_elision       = false,
    .inlining               = false,
    .unused_message_elision = false,
    .loop_errors            = false,
};

const char *BLIMP_OPTIONS_USAGE =
    "    recursion-limit=N\n"
    "    no-recursion-limit\n"
    "        The first form sets the maximum number of message sends allowed\n"
    "        on the stack at a time. The second form disables a previously\n"
    "        set limit.\n"
    "\n"
    "        If this option is enabled, then attempts to evaluate a message\n"
    "        send when there are already N sends on the stack will fail. Note\n"
    "        that even if this option is disabled, the allowed recursion\n"
    "        depth may be limited by the size of the process stack.\n"
    "\n"
    "        The default is 1000\n"
    "\n"
    "    stack-trace-limit=N\n"
    "    no-stack-trace-limit\n"
    "        The first form sets a limit on how much of large stack traces is\n"
    "        printed when a runtime error occurs. The second form disables a\n"
    "        previously set limit.\n"
    "\n"
    "        If this option is enabled, then when a stack trace is printed,\n"
    "        at most N frames are printed from the beginning of the trace,\n"
    "        and at most N frames are printed from the end of the trace.\n"
    "        Missing frames will be indicated with an ellipsis, as well as by\n"
    "        a discontinuity in the stack depth printed with each frame.\n"
    "\n"
    "        If this option is disabled, the entire stack trace will be\n"
    "        printed.\n"
    "\n"
    "        The default is 5.\n"
    "\n"
    "    gc-batch-size=SIZE\n"
    "        When allocating new objects, this option controls the number of\n"
    "        objects allocated at once, to be initialized on demand later.\n"
    "        Specifically, SIZE is the number of bytes worth of objects\n"
    "        allocated at a time.\n"
    "\n"
    "        Tuning this paramter adjusts the tradeoff between excess memory\n"
    "        usage (when SIZE is larger than necessary) and high allocation\n"
    "        latency (when SIZE is small). Since most bl:mp programs allocate\n"
    "        and free many short-lived objects rapidly, overall performance\n"
    "        is very sensitive to allocation latency. If you're interested in\n"
    "        tuning performance, you should try to set this parameter as\n"
    "        large as your memory constraints will allow.\n"
    "\n"
    "        SIZE may be specified in any of the following forms (`n' denotes\n"
    "        a positive integer literal):\n"
    "           * n\n"
    "           * nKB\n"
    "           * nMB\n"
    "           * nGB\n"
    "        The default is 1MB.\n"
    "\n"
    "    [no-]gc-tracing\n"
    "        Enable or disable tracing garbage collection.\n"
    "\n"
    "        Only tracing garbage collection can reliably free memory\n"
    "        allocated to reference cycles which are no longer reachable.\n"
    "\n"
    "        If this option is set, the tracing garbage collector will run\n"
    "        periodically when a new object is needed and no previously freed\n"
    "        objects are available. `gc_batches_per_trace` can be used to\n"
    "        configure how often the system uses garbage collection to find\n"
    "        free objects (which can bevery expensive) versus how often it\n"
    "        allocates new batches when it needs a free object (which is\n"
    "        comparatively less expensive, but can result in greater high\n"
    "        water marks for memory usage).\n"
    "\n"
    "        If this option is not set, the system will not attempt to free\n"
    "        reference cycles. It will continue allocating new batches of\n"
    "        objects when free objects are not available until the underlying\n"
    "        allocator runs out of memory.\n"
    "\n"
    "        Tracing garbage collection is enabled by default.\n"
    "\n"
    "    gc-batches-per-trace=N\n"
    "        This option controls how often the tracing garbage collector\n"
    "        runs, if it is enabled.\n"
    "\n"
    "        The system will allocate N batches of objects between each run\n"
    "        of the tracing garbage collector. The collector will only run if\n"
    "        a new object is needed, there are no free objects available, and\n"
    "        we have  allocated N batches since the last GC sweep. Otherwise,\n"
    "        we will allocate a new batch without running the tracing GC.\n"
    "\n"
    "        The default is 1.\n"
    "\n"
    "    [no-]gc-refcount\n"
    "        Enable or disable internal reference counting.\n"
    "\n"
    "        Reference counting for managed references is always enabled,\n"
    "        because the tracing garbage collector uses it to determine which\n"
    "        objects the user of the API is currently using, and which\n"
    "        objects are reachable from those.\n"
    "\n"
    "        This option affects reference counting for internal references:\n"
    "        references between objects through their scopes and parents. If\n"
    "        internal reference counting is enabled, the system will\n"
    "        sometimes be able to free objects without invoking the tracing\n"
    "        garbage collector by determining that an object is not only\n"
    "        unreferenced by the user, but it is also unreferenced by any\n"
    "        other object. This can improve performance in certain\n"
    "        applications.\n"
    "        \n"
    "        This option is enabled by default.\n"
    "\n"
    "    [no-]gc-cycle-detection\n"
    "        Enable or disable enhanced reference counting.\n"
    "\n"
    "        Standard internal reference counting (gc_refcount) is able to\n"
    "        free unreachable objects which are not referenced by other\n"
    "        objects in a cycle, without invoking the tracing garbage\n"
    "        collector. Enhanced reference counting extends this capability\n"
    "        with limited cycle detection.\n"
    "\n"
    "        Specifically, cycles formed by a single scope reference and one\n"
    "        or more parent references can be detected and collected without\n"
    "        using the tracing garbage collector.\n"
    "\n"
    "        This allows the system to collect certain very common data\n"
    "        structure patterns using reference counting. For example,\n"
    "        consider the case of a simple local variable:\n"
    "\n"
    "            {\n"
    "                local{^ value}\n"
    "            }\n"
    "\n"
    "        The outer block object clearly has a reference to `value`\n"
    "        through `local` in its scope. But `value` has a reference to the\n"
    "        inner block object, which is its parent. And that object has a\n"
    "        reference to the outer block object, which is _its_ parent,\n"
    "        creating a cycle. `gc_refcount` on its own would not be able to\n"
    "        free any of the objects involved in this cycle; we would need\n"
    "        `gc_tracing` to do so. But with `gc_cycle_detection` enabled, we\n"
    "        will detect that these objects form a cycle, and we will free\n"
    "        the whole group of objects as soon as all references to objects\n"
    "        in the cycle originate from within the cycle; that is, as soon\n"
    "        as there are no references from outside the cycle to any of the\n"
    "        objects in it.\n"
    "\n"
    "        This option is enabled by default.\n"
    "\n"
    "    gc-max-clump-size=N\n"
    "    no-gc-max-clump-size\n"
    "        The first form sets a limit on the size of ERC clumps. The\n"
    "        second form disables a previously set limit.\n"
    "\n"
    "        If this option is enabled, then clumps larger than N objects\n"
    "        will stop entangling with new objects. Large clumps can be\n"
    "        can be expensive to manage, and each new object entangled with a\n"
    "        clump decreases the probability that that clump will ever be\n"
    "        freed by ERC, so limiting the clump size can sometimes be a\n"
    "        performance optimization.\n"
    "\n"
    "        In addition, objects created at different times, more than N\n"
    "        objects apart, are prohibited from entangling. This means that\n"
    "        if an object is particularly long-lived, once N objects have\n"
    "        been created after it, no new objects will entangle with that\n"
    "        object. This can prevent short-lived objects from entangling\n"
    "        with long-lived objects, which could prevent the short-lived\n"
    "        objects from being freed.\n"
    "\n"
    "        This option is disabled by default.\n"
    "\n"
    "    [no-]gc-heap-check\n"
    "        Enable or disable runtime heap checking.\n"
    "\n"
    "        If the bl:mp interpreter was built with debugging turned on and\n"
    "        this option is set, then the interpreter will periodically sweep\n"
    "        the heap looking for incosistencies and abort if it finds one.\n"
    "        This can be used to detect bugs in the built-in garbage\n"
    "        collector.\n"
    "\n"
    "        Enabling this option will significantly slow down execution.\n"
    "\n"
    "        This option is disabled by default.\n"
    "\n"
    "    [no-]tail-call-elimination\n"
    "        Enable or disable tail call elimination.\n"
    "\n"
    "        This option is enabled by default.\n"
    "\n"
    "    [no-]constant-elision\n"
    "        Enable or disable constant elision.\n"
    "\n"
    "        Constant elision is an optimization which allows the interpreter\n"
    "        to detect when a symbol receiving a message is constant, and\n"
    "        replace the code with bytecode that sends the message directly\n"
    "        to the value of the symbol. This saves time by eliminating the\n"
    "        need to look up the constant value of the symbol each time that\n"
    "        code executes. It also serves as a gateway to further\n"
    "        optimization, such as inlining.\n"
    "\n"
    "        This option is disabled by default.\n"
    "\n"
    "    [no-]inlining\n"
    "        Enable or disable inlining.\n"
    "\n"
    "        When enabled, the interpreter may inline message sends at its\n"
    "        discretion, effectively replacing a call to an object's message\n"
    "        handler with a copy of that message handler's code. This reduces\n"
    "        the overhead of sending messages, and it allows further\n"
    "        optimization because the inlined copy of the code can be\n"
    "        optimized in the context in which it is inlined, where, among\n"
    "        other things, the interpreter knows what the message is going to\n"
    "        be.\n"
    "\n"
    "        This optimization is most effective when used in conjunction\n"
    "        with constant elision, because message sends to an object a\n"
    "        through symbol can only be inlined if the value of the symbol\n"
    "        is first determined to be a constant.\n"
    "\n"
    "        This option is disabled by default.\n"
    "\n"
    "    [no-]unused-message-elision\n"
    "        Enable or disable the optimization of unused messages.\n"
    "\n"
    "        When enabled, if a message is sent to an object that the\n"
    "        interpreter can determine does not use its message, then the\n"
    "        computation of the message can be elided (if it has no side-\n"
    "        effects) or decoupled from the sending of the message. This in\n"
    "        turn can enable inlining of the receiver (if inlining is\n"
    "        enabled) even in the case when the message has side-effects.\n"
    "\n"
    "        This option is disabled by default.\n"
    "\n"
    "    [no-]loop-errors\n"
    "        Treat infinite loops as errors.\n"
    "\n"
    "        When enabled, the interpreter may return an error if it detects\n"
    "        that a computation will result in evaluating an infinite loop.\n"
    "        This differs from the standard behavior, where the interpreter\n"
    "        would simply execute the loop forever, never returning.\n"
    "  \n"
    "        Note that not all infinite loops can be easily detected, so the\n"
    "        interpreter is conservative: it will only report those loops\n"
    "        which it can detect efficiently, and it will never report an\n"
    "        infinite loop where there isn't one.\n"
    "  \n"
    "        This option is disabled by default.\n"
;

static const char *ParseUInt(const char *value, size_t *result)
{
    if (!*value) {
        return "option requires an argument";
    }

    char *invalid;
    long n = strtol(value, &invalid, 0);
    if (*invalid) {
        return "option requires a numeric argument";
    }
    if (n < 0) {
        return "argument must not be negative";
    }

    *result = n;
    return NULL;
}

static const char *ParseBytes(const char *value, size_t *result)
{
    #define bytes 1
    #define kb    ((bytes) << 10)
    #define mb    ((kb)    << 10)
    #define gb    ((mb)    << 10)

    if (!*value) {
        return "option requires an argument";
    }

    char *unit_str;
    long scalar = strtol(value, &unit_str, 0);
    if (unit_str == value) {
        return "option requires a numeric argument";
    }
    if (scalar < 0) {
        return "argument must not be negative";
    }

    size_t units;
    if (strcmp("", unit_str) == 0) {
        units = bytes;
    } else if (strcmp("KB", unit_str) == 0) {
        units = kb;
    } else if (strcmp("MB", unit_str) == 0) {
        units = mb;
    } else if (strcmp("GB", unit_str) == 0) {
        units = gb;
    } else {
        return "invalid units";
    }

    *result = scalar*units;
    return NULL;
}

const char *Blimp_ParseOption(const char *str, BlimpOptions *options)
{

    size_t option_len = 0;
    const char *option = str;

    // Check for a `no-` prefix.
    bool negate = strncmp("no-", str, 3) == 0;
    if (negate) {
        option += 3;
    }

    // Split the string at the first occurrence of '=', if there is one.
    const char *value = str;
    while (*value && *value++ != '=') {
        ++option_len;
    }

    // We can't mix boolean options and scalar options.
    if (negate && *value) {
        return "invalid combination of no- and =";
    }

    // Option-specific handling.
    if        (strncmp("recursion-limit", option, option_len) == 0) {
        if (negate) {
            options->recursion_limit = 0;
            return NULL;
        } else {
            return ParseUInt(value, &options->recursion_limit);
        }
    } else if (strncmp("stack-trace-limit", option, option_len) == 0) {
        if (negate) {
            options->stack_trace_limit = 0;
            return NULL;
        } else {
            return ParseUInt(value, &options->stack_trace_limit);
        }
    } else if (strncmp("gc-batch-size", option, option_len) == 0) {
        return ParseBytes(value, &options->gc_batch_size);
    } else if (strncmp("gc-tracing", option, option_len) == 0) {
        options->gc_tracing = !negate;
        return NULL;
    } else if (strncmp("gc-batches-per-trace", option, option_len) == 0) {
        return ParseUInt(value, &options->gc_batches_per_trace);
    } else if (strncmp("gc-refcount", option, option_len) == 0) {
        options->gc_refcount = !negate;
        return NULL;
    } else if (strncmp("gc-cycle-detection", option, option_len) == 0) {
        options->gc_cycle_detection = !negate;
        return NULL;
    } else if (strncmp("gc-max-clump-size", option, option_len) == 0) {
        if (negate) {
            options->gc_max_clump_size = 0;
            return NULL;
        } else {
            return ParseUInt(value, &options->gc_max_clump_size);
        }
    } else if (strncmp("gc-heap-check", option, option_len) == 0) {
        options->gc_heap_check = !negate;
        return NULL;
    } else if (strncmp("tail-call-elimination", option, option_len) == 0) {
        options->tail_call_elimination = !negate;
        return NULL;
    } else if (strncmp("constant-elision", option, option_len) == 0) {
        options->constant_elision = !negate;
        return NULL;
    } else if (strncmp("inlining", option, option_len) == 0) {
        options->inlining = !negate;
        return NULL;
    } else if (strncmp("unused-message-elision", option, option_len) == 0) {
        options->unused_message_elision = !negate;
        return NULL;
    } else if (strncmp("loop-errors", option, option_len) == 0) {
        options->loop_errors = !negate;
        return NULL;
    } else {
        return "unknown option";
    }
}

void Blimp_OptimizationsOn(BlimpOptions *options)
{
    options->tail_call_elimination = true;
    options->constant_elision = true;
    options->inlining = true;
    options->unused_message_elision = true;
}
