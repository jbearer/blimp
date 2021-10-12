#ifndef TEST_OPTIONS
#define TEST_OPTIONS

#include <regex.h>
#include <stdbool.h>
#include <stdlib.h>

#include <blimp.h>

typedef enum {
    VERB_NONE,
    VERB_SUITE,
    VERB_GROUP,
    VERB_FAILURES,
    VERB_TEST,
    VERB_SKIPPED,
    VERB_STATS,
    VERB_DEBUG,
    MAX_VERBOSITY,
} Verbosity;

typedef struct {
    Verbosity verbosity;
    const char **tests;
    size_t num_tests;
    const char **groups;
    size_t num_groups;
    regex_t filter;
    bool enable_racket;
    bool use_racket;
    bool use_blimp;
    size_t racket_timeout;  // ms
    size_t blimp_timeout;   // ms
    float perf_factor;
    const char **preimport;
    size_t num_preimport;
    const char **preload;
    size_t num_preload;
    BlimpOptions blimp_options;
    FILE *perf_report;
} Options;

#endif
