#include "internal/blimp.h"

const BlimpOptions DEFAULT_BLIMP_OPTIONS = {
    .object_pool_batch_size = (1ull<<20), // 1MB
};

const char *BLIMP_OPTIONS_USAGE =
    "    object-pool-batch-size=SIZE\n"
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
;

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
    if (strncmp("object-pool-batch-size", option, option_len) == 0) {
        return ParseBytes(value, &options->object_pool_batch_size);
    } else {
        return "unknown option";
    }
}
