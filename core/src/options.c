#include "internal/blimp.h"

const BlimpOptions DEFAULT_BLIMP_OPTIONS = {
    .gc_batch_size        = (1ull<<20), // 1MB
    .gc_tracing           = true,
    .gc_batches_per_trace = 1,
    .gc_refcount          = true,
    .gc_cycle_detection   = true,
};

const char *BLIMP_OPTIONS_USAGE =
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
    "       If this option is not set, the system will not attempt to free\n"
    "       reference cycles. It will continue allocating new batches of\n"
    "       objects when free objects are not available until the underlying\n"
    "       allocator runs out of memory.\n"
    "\n"
    "       Tracing garbage collection is enabled by default.\n"
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
    "             {outer_scope|\n"
    "                 local{:=|value}\n"
    "             }\n"
    "\n"
    "        The `outer_scope` object clearly has a reference to `value`\n"
    "        through `local` in its scope. But `value` has a reference to\n"
    "        the `:=` object, which is its parent. And that object has a\n"
    "        reference to the `outer_scope` object, creating a cycle.\n"
    "        `gc_refcount` on its own would not be able to free any of the\n"
    "        objects involved in this cycle; we would need`gc_tracing` to do\n"
    "        so. But with `gc_cycle_detection` enabled, we will detect that\n"
    "        these objects form a cycle, and we will free the whole group of\n"
    "        objects as soon as all references to objects in the cycle\n"
    "        originate from within the cycle; that is, as soon as there are\n"
    "        no references from outside the cycle to any of the objects in\n"
    "        it.\n"
    "        \n"
    "        This option is enabled by default.\n"
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
    if        (strncmp("gc-batch-size", option, option_len) == 0) {
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
    } else {
        return "unknown option";
    }
}
