#include "internal/blimp.h"

const BlimpOptions DEFAULT_BLIMP_OPTIONS = {
    .gc_batch_size        = (1ull<<20), // 1MB
    .gc_tracing           = true,
    .gc_batches_per_trace = 1,
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
    } else {
        return "unknown option";
    }
}
