#ifndef BLIMP_TEST_H
#define BLIMP_TEST_H

#include <wordexp.h>

#include "options.h"
#include "racket.h"

typedef enum {
    TEST_PASSED,
    TEST_PERF_REGRESSION,
    TEST_FAILED,
    TEST_SKIPPED,
    NUM_RESULT_TYPES,
} TestResult;

typedef struct Test {
    // Inputs
    const char *name;
    struct Group *group;
    Options options;
    wordexp_t options_split;
    Blimp *blimp;
    BlimpStream *stream;
    Racket *racket;

    // Outputs
    TestResult result;
} Test;

typedef struct Group {
    // Inputs
    const char *name;
    Options options;
    size_t num_tests;
    Test **tests;
    char *import_path[2];

    // Outputs
    size_t results[NUM_RESULT_TYPES];
} Group;

typedef struct Suite {
    size_t num_groups;
    Group **groups;
    Racket racket;
    Options options;
} Suite;

#endif
