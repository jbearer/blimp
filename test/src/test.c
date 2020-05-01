#define _POSIX_C_SOURCE 200809L

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>

#include "blimp.h"
#include "options.h"
#include "racket.h"

#define ANSI_GREEN  "\e[1;32m"
#define ANSI_RED    "\e[1;31m"
#define ANSI_PURPLE "\e[1;35m"
#define ANSI_YELLOW "\e[1;33m"
#define ANSI_RESET  "\e[0m"

typedef enum {
    TEST_PASSED,
    TEST_FAILED,
    TEST_SKIPPED,
    NUM_RESULT_TYPES,
} TestResult;

typedef struct Test {
    // Inputs
    const char *name;
    Blimp *blimp;
    BlimpStream *stream;
    struct Suite *suite;

    // Outputs
    TestResult result;
} Test;

typedef struct Group {
    // Inputs
    const char *name;
    size_t num_tests;
    struct Test **tests;
    struct Group *next;
    struct Suite *suite;

    // Outputs
    size_t results[NUM_RESULT_TYPES];
} Group;

typedef struct Suite {
    Group *groups;
    Racket racket;
    Options options;
} Suite;

////////////////////////////////////////////////////////////////////////////////
// Displaying results
//

static void FailTest(Test *test, const char *reason)
{
    if (test->suite->options.verbosity >= VERB_TEST) {
        printf(ANSI_RED "failed!" ANSI_RESET " %s: %s\n", test->name, reason);
    }
    test->result = TEST_FAILED;
}

static void PassTest(Test *test, size_t elapsed_ms)
{
    if (test->suite->options.verbosity >= VERB_TEST) {
        printf(ANSI_GREEN "passed!" ANSI_RESET " %s (%.3fs)\n",
            test->name, (float)elapsed_ms / 1000);
    }
    test->result = TEST_PASSED;
}

// Print a summary of a group of tests.
//  header: a format string describing the results being summarized
//  results:
//      an array of NUM_RESULT_TYPES size_t's. For each kind of TestResult,
//      results[i] should indicate the number of tests which had that result.
//  num_tests:
//      the total number of tests run (this should be the sum of `results`), but
//      that is not checked.
//  ...: arguments used to format `header`
#define PrintResults(header, results, num_tests, ...) \
    printf(header ": %s%zu%s passed / %s%zu%s failed / %s%zu%s skipped / %zu total\n", \
        ##__VA_ARGS__, \
 \
        results[TEST_PASSED] ? ANSI_GREEN : "", \
        results[TEST_PASSED], \
        results[TEST_PASSED] ? ANSI_RESET : "", \
 \
        results[TEST_FAILED] ? ANSI_RED : "", \
        results[TEST_FAILED], \
        results[TEST_FAILED] ? ANSI_RESET : "", \
 \
        results[TEST_SKIPPED] ? ANSI_YELLOW : "", \
        results[TEST_SKIPPED], \
        results[TEST_SKIPPED] ? ANSI_RESET : "", \
 \
         num_tests \
    )

////////////////////////////////////////////////////////////////////////////////
// Running tests
//

static void RunTest(Test *test)
{
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);

    BlimpExpr *expr;
    if (Blimp_Parse(test->blimp, test->stream, &expr) != BLIMP_OK) {
        FailTest(test, "failed to parse");
        return;
    }

    if (test->suite->options.use_racket) {
        FILE *command = Racket_BeginCommand(&test->suite->racket);
        fprintf(command, "(judgment-holds (test-eval ");
        Blimp_DumpExpr(command, expr);
        fprintf(command, " M v))");
        char *output = Racket_CommitCommand(
            &test->suite->racket, test->suite->options.racket_timeout);
        if (!output) {
            FailTest(test, "Racket error: expected output");
            return;
        }

        size_t output_len = strlen(output);
        if (output_len && output[output_len-1] == '\n') {
            output[--output_len] = '\0';
        }
        if (strcmp(output, "#t") != 0) {
            // Got an unexpected response.
            FailTest(test, "Racket error: judgment did not hold");
            free(output);
            return;
        }
        free(output);
    }

    clock_gettime(CLOCK_MONOTONIC, &end);

    size_t elapsed_ns = (end  .tv_sec*1000000000 + end  .tv_nsec) -
                        (start.tv_sec*1000000000 + start.tv_nsec);
    PassTest(test, elapsed_ns/1000000);
}

static void RunGroup(Group *group)
{
    memset(group->results, 0, sizeof(group->results));

    if (group->suite->options.verbosity >= VERB_TEST) {
        printf(ANSI_PURPLE "Running tests for group" ANSI_RESET " %s\n",
            group->name);
    }

    for (size_t i = 0; i < group->num_tests; ++i) {
        RunTest(group->tests[i]);
        ++group->results[group->tests[i]->result];
    }

    if (group->suite->options.verbosity >= VERB_GROUP) {
        PrintResults(ANSI_PURPLE "Results for" ANSI_RESET " %s",
            group->results, group->num_tests, group->name);
    }
    if (group->suite->options.verbosity >= VERB_TEST) {
        printf("\n");
    }
}

static bool RunSuite(Suite *suite)
{
    if (suite->options.use_racket) {
        if (!Racket_Init(&suite->racket, &suite->options)) {
            if (suite->options.verbosity >= VERB_SUITE) {
                fprintf(stderr,
                    "failed to open Racket "
                    "(maybe you meant to run with --skip-racket)\n");
            }
            return false;
        }

        if (!Racket_Exec(&suite->racket, "(require redex)")) {
            if (suite->options.verbosity >= VERB_SUITE) {
                fprintf(stderr, "racket: failed to import redex\n");
            }
            return false;
        }
        if (!Racket_Exec(&suite->racket, "(require (file \"" SEMANTICS_PATH "\"))")) {
            if (suite->options.verbosity >= VERB_SUITE) {
                fprintf(stderr, "racket: failed to import semantics.rkt\n");
            }
            return false;
        }
    }

    size_t results[NUM_RESULT_TYPES] = {0};
    size_t num_tests = 0;
    for (Group *group = suite->groups; group; group = group->next) {
        RunGroup(group);
        for (TestResult result = 0; result < NUM_RESULT_TYPES; ++result) {
            results[result] += group->results[result];
        }
        num_tests += group->num_tests;
    }

    if (suite->options.verbosity >= VERB_SUITE) {
        PrintResults(ANSI_PURPLE "Total results" ANSI_RESET, results, num_tests);
    }
    return results[TEST_FAILED] == 0;
}

////////////////////////////////////////////////////////////////////////////////
// Discovering tests
//

static int CompareTests(const void *p1, const void *p2)
{
    Test *t1 = *(Test **)p1;
    Test *t2 = *(Test **)p2;
    return strcmp(t1->name, t2->name);
}

static Suite *FindTests(const Options *options)
{
    Suite *suite = malloc(sizeof(Suite));
    suite->options = *options;
    suite->groups = NULL;

    DIR *suite_dir = opendir(TEST_DIRECTORY);
    if (!suite_dir) {
        perror("opendir");
        return NULL;
    }

    // Search each subdirectory of the test directory for .blt files. Any
    // subdirectory containing at least one .blt fil will become a group.
    struct dirent *group_de;
    while ((group_de = readdir(suite_dir)) != NULL) {
        int group_dir_fd = openat(
            dirfd(suite_dir), group_de->d_name, O_RDONLY|O_DIRECTORY);
        if (group_dir_fd < 0) {
            if (errno == ENOTDIR) {
                continue;
            }

            fprintf(stderr, "could not open test directory %s: %s\n",
                group_de->d_name, strerror(errno));
        }

        Test **tests = NULL;
        size_t tests_capacity = 0;
        size_t num_tests = 0;

        DIR *group_dir = fdopendir(group_dir_fd);
        struct dirent *test_de;
        while ((test_de = readdir(group_dir)) != NULL) {
            // Is this a .blt file?
            size_t name_len = strlen(test_de->d_name);
            if (name_len < 4 ||
                strcmp(test_de->d_name + name_len - 4, ".blt") != 0)
            {
                continue;
            }

            // It is! Does it match our filter?
            if (strncasecmp(
                    test_de->d_name, options->filter, strlen(options->filter))
                != 0)
            {
                continue;
            }

            // It is! Create a test for it.
            Test *test = malloc(sizeof(Test));
            test->name = strdup(test_de->d_name);
            test->blimp = Blimp_New();
            FILE *test_file = fdopen(
                openat(dirfd(group_dir), test_de->d_name, O_RDONLY), "r");
            Blimp_Check(Blimp_OpenFileStream(
                test->blimp, test_de->d_name, test_file, &test->stream));
            test->suite = suite;

            // Add it to our list of tests.
            if (num_tests >= tests_capacity) {
                tests_capacity = tests_capacity*2 + 1;
                tests = realloc(tests, sizeof(Test *)*tests_capacity);
            }
            tests[num_tests++] = test;
        }

        // If we found any tests in this directory, create a group for them.
        if (tests) {
            // Sort the tests alphabetically.
            qsort(tests, num_tests, sizeof(Test *), CompareTests);

            Group *group = malloc(sizeof(Group));
            group->name = strdup(group_de->d_name);
            group->num_tests = num_tests;
            group->tests = tests;
            group->next = suite->groups;
            group->suite = suite;
            suite->groups = group;
        }
    }

    return suite;
}

////////////////////////////////////////////////////////////////////////////////
// Command-line option processing
//
// Each option must be registered in six places:
//  1. The help string in PrintUsage
//  2. The Flag enum
//  3. The Options struct in options.h
//  4. The `default_options` global constant
//  5. The `cli_options` array in ParseOptions
//  6. The switch statement in ParseOptions
//

static void PrintUsage(FILE *f, int argc, char *const *argv)
{
    (void)argc;

    fprintf(f, "Usage: %s [options]\n", argv[0]);
    fprintf(f, "Options:\n");
    fprintf(f, "\n");
    fprintf(f, "    -f, --filter STRING\n");
    fprintf(f, "        Run only tests whose name begins with STRING (case-insensitive).\n");
    fprintf(f, "\n");
    fprintf(f, "    --skip-racket\n");
    fprintf(f, "        Do not run Racket semantics tests.\n");
    fprintf(f, "\n");
    fprintf(f, "    --racket-timeout SECONDS\n");
    fprintf(f, "        Consider a test failed if it takes more than SECONDS to evaluate\n");
    fprintf(f, "        in the Redex semantic model. SECONDS may be an integer or floating\n");
    fprintf(f, "        point literal. The default is 5.\n");
    fprintf(f, "\n");
    fprintf(f, "    -v, --verbose [LEVEL]\n");
    fprintf(f, "        Show verbose output at LEVEL. LEVEL may be one of the following\n");
    fprintf(f, "        (each named verbosity level implies the level below it):\n");
    fprintf(f, "         * debug: show output useful for debugging the test runner\n");
    fprintf(f, "         * test:  show output for each test run\n");
    fprintf(f, "         * group: show output for each group of tests\n");
    fprintf(f, "         * suite: show summary output for the entire test suite\n");
    fprintf(f, "         * none: do not show any output\n");
    fprintf(f, "         * N (a non-negative integer)\n");
    fprintf(f, "        If no verbose option is given, the verbosity is set to group.\n");
    fprintf(f, "        If the verbose flag is given with no argument, the verbosity is set\n");
    fprintf(f, "        to `test'.\n");
    fprintf(f, "\n");
    fprintf(f, "    -h, --help\n");
    fprintf(f, "        Show this help and exit.\n");
}

// Symbolic identifiers for command-line options.
//
// For options which have a short alias (e.g. -h for --help) the value of the
// corresponding symbol should be set explicitly to that short alias, so that
// `getopt_long` will return that value regardless of whether the short or long
// name is given on the command line.
//
// For long-only options, do not specify an explicit value, so that the compiler
// will automatically choose a unique name. Long options with no corresponding
// short option should be registered after FLAG_NO_SHORT_OPTION.
typedef enum {
    FLAG_FILTER             = 'f',
    FLAG_VERBOSE            = 'v',
    FLAG_HELP               = 'h',

    FLAG_NO_SHORT_OPTION    = 'z'+1,
        // Dummy option which should be greater than all the short options
        // specified above. Any flag declared after this sentinel with no
        // explicit short option value will be automatically assigned a value
        // greater than the value of the sentinel, which should guarantee that
        // all the flags have unique values.

    FLAG_SKIP_RACKET,
    FLAG_RACKET_TIMEOUT,
} Flag;

const Options default_options = {
    .verbosity      = VERB_GROUP,
    .filter         = "",
    .use_racket     = true,
    .racket_timeout = 5000,
};

// Parse the options in the given command line, and store the result in the
// `options` structure. `options` is an in-out parameter: if a given option is
// not specified, it will default to the value assigned to that field in
// `options` when this function is called.
//
// The return value indicates whether the given options should cause immediate
// termination of the program. If the return value is `true`, and `status` is
// non-NULL, then `status` will point to an integer which should be the exit
// status of the program.
bool ParseOptions(int argc, char **argv, Options *options, int *status)
{
    struct option cli_options[] = {
        {"filter",         required_argument, NULL, FLAG_FILTER },
        {"skip-racket",    no_argument,       NULL, FLAG_SKIP_RACKET },
        {"racket-timeout", required_argument, NULL, FLAG_RACKET_TIMEOUT },
        {"verbose",        optional_argument, NULL, FLAG_VERBOSE },
        {"help",           no_argument,       NULL, FLAG_HELP },
        {0, 0, 0, 0},
    };

    int option, i = 1;
    while ((option = getopt_long(argc, argv, "f:v::h", cli_options, &i)) != -1) {
        switch (option) {
            case FLAG_FILTER:
                options->filter = optarg;
                break;

            case FLAG_SKIP_RACKET:
                options->use_racket = false;
                break;

            case FLAG_RACKET_TIMEOUT: {
                char *invalid;
                float seconds = strtof(optarg, &invalid);
                if (*invalid) {
                    fprintf(stderr,
                        "racket-timeout: argument must be a number\n");
                    PrintUsage(stderr, argc, argv);
                    *status = EXIT_FAILURE;
                    return true;
                }
                options->racket_timeout = seconds*1000;

                break;
            }

            case FLAG_VERBOSE:
                if (optarg == NULL) {
                    options->verbosity = VERB_GROUP;
                    break;
                }

                // Try to parse the argument as an integer.
                char *invalid;
                long level = strtol(optarg, &invalid, 0);
                if (*optarg && !*invalid) {
                    // The entire string was a valid integer.
                    if (level < 0) {
                        level = 0;
                    } else if (level >= MAX_VERBOSITY) {
                        level = MAX_VERBOSITY - 1;
                    }
                    options->verbosity = (Verbosity)level;
                    break;
                }

                // Try to parse the argument as the name of a verbosity level.
                if (strcmp(optarg, "debug") == 0) {
                    options->verbosity = VERB_DEBUG;
                } else if (strcmp(optarg, "test") == 0) {
                    options->verbosity = VERB_TEST;
                } else if (strcmp(optarg, "group") == 0) {
                    options->verbosity = VERB_GROUP;
                } else if (strcmp(optarg, "suite") == 0) {
                    options->verbosity = VERB_SUITE;
                } else if (strcmp(optarg, "none") == 0) {
                    options->verbosity = VERB_NONE;
                } else {
                    fprintf(stderr, "invalid verbosity level\n");
                    PrintUsage(stderr, argc, argv);
                    *status = EXIT_FAILURE;
                    return true;
                }

                break;

            case FLAG_HELP:
                PrintUsage(stdout, argc, argv);
                *status = EXIT_SUCCESS;
                return true;
            default:
                PrintUsage(stderr, argc, argv);
                *status = EXIT_FAILURE;
                return true;
        }
    }

    return false;
}

int main(int argc, char **argv)
{
    Options options = default_options;
    int status;
    bool should_exit = ParseOptions(argc, argv, &options, &status);
    if (should_exit) {
        return status;
    }

    if (RunSuite(FindTests(&options))) {
        return EXIT_SUCCESS;
    } else {
        return EXIT_FAILURE;
    }
}
