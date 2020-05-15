#ifndef _POSIX_C_SOURCE
# define _POSIX_C_SOURCE 200809L
#endif

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
#include <wordexp.h>

#include "blimp.h"
#include "options.h"
#include "racket.h"
#include "test_blimp.h"

#define ANSI_GREEN  "\e[1;32m"
#define ANSI_RED    "\e[1;31m"
#define ANSI_PURPLE "\e[1;35m"
#define ANSI_YELLOW "\e[1;33m"
#define ANSI_RESET  "\e[0m"

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
    Options options;
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

    // Outputs
    size_t results[NUM_RESULT_TYPES];
} Group;

typedef struct Suite {
    size_t num_groups;
    Group **groups;
    Racket racket;
    Options options;
} Suite;

////////////////////////////////////////////////////////////////////////////////
// Displaying results
//

static void FailTest(Test *test, const char *reason)
{
    if (test->options.verbosity >= VERB_FAILURES) {
        printf(ANSI_RED "failed!" ANSI_RESET " %s: %s\n", test->name, reason);
    }
    test->result = TEST_FAILED;
}

static void PassTest(Test *test, size_t racket_ms, size_t blimp_ms)
{
    float expected_ms = test->options.perf_factor*test->options.blimp_timeout;

    // Check for performance regressions.
    if (test->options.use_blimp && blimp_ms > expected_ms)
    {
        printf(ANSI_YELLOW "performance regression!" ANSI_RESET
               " %s: performance regression: %.3f > %.3f\n",
               test->name,
               (float)blimp_ms/1000, expected_ms/1000);
        test->result = TEST_PERF_REGRESSION;
        return;
    }

    if (test->options.verbosity >= VERB_TEST) {
        printf(ANSI_GREEN "passed!" ANSI_RESET " %s", test->name);

        // Print timing information.
        if (test->options.use_blimp && test->options.use_racket) {
            printf(" (%.3fs racket, %.3fs bl:mp)",
                (float)racket_ms / 1000, (float)blimp_ms / 1000);
        } else if (test->options.use_blimp) {
            printf(" (%.3fs)", (float)blimp_ms / 1000);
        } else if (test->options.use_racket) {
            printf(" (%.3fs)", (float)racket_ms / 1000);
        }

        printf("\n");
    }
    test->result = TEST_PASSED;
}

#define SkipTest(test, reason, ...) \
    do { \
        if (test->options.verbosity >= VERB_TEST) { \
            printf("skipped! %s: " reason "\n", test->name, ##__VA_ARGS__); \
        } \
        test->result = TEST_SKIPPED; \
    } while (0)

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
    printf(header ": %s%zu%s passed / %s%zu%s failed / %s%zu%s performance / %zu skipped / %zu total\n", \
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
        results[TEST_PERF_REGRESSION] ? ANSI_YELLOW : "", \
        results[TEST_PERF_REGRESSION], \
        results[TEST_PERF_REGRESSION] ? ANSI_RESET : "", \
\
        results[TEST_SKIPPED], \
\
        num_tests \
    )

////////////////////////////////////////////////////////////////////////////////
// Running tests
//

static void RunTest(Test *test)
{
    // Skip this test if it gets filtered out.
    if (strncasecmp(
            test->name, test->options.filter, strlen(test->options.filter))
        != 0)
    {
        SkipTest(test, "does not match filter `%s'", test->options.filter);
        goto cleanup;
    }

    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);

    BlimpExpr *expr;
    if (Blimp_Parse(test->blimp, test->stream, &expr) != BLIMP_OK) {
        FailTest(test, "failed to parse");
        if (test->options.verbosity >= VERB_FAILURES) {
            Blimp_DumpLastError(test->blimp, stdout);
        }
        goto cleanup;
    }

    if (test->options.use_racket) {
        FILE *command = Racket_BeginCommand(test->racket);
        fprintf(command, "(judgment-holds (test-eval ");
        Blimp_DumpExpr(command, expr);
        fprintf(command, " M v))");
        char *output = Racket_CommitCommand(
            test->racket,
            test->options.perf_factor*test->options.racket_timeout
        );
        if (!output) {
            FailTest(test, "Racket error: expected output");
            goto cleanup_parsed;
        }

        size_t output_len = strlen(output);
        if (output_len && output[output_len-1] == '\n') {
            output[--output_len] = '\0';
        }
        if (strcmp(output, "#t") != 0) {
            // Got an unexpected response.
            FailTest(test, "Racket error: judgment did not hold");
            free(output);
            goto cleanup_parsed;
        }
        free(output);
    }

    clock_gettime(CLOCK_MONOTONIC, &end);
    size_t racket_ns = (end  .tv_sec*1000000000 + end  .tv_nsec) -
                       (start.tv_sec*1000000000 + start.tv_nsec);

    clock_gettime(CLOCK_MONOTONIC, &start);

    BlimpObject *result = NULL;
    if (test->options.use_blimp) {
        if (Blimp_Eval(
                test->blimp, expr, Blimp_GlobalObject(test->blimp), &result)
            != BLIMP_OK)
        {
            FailTest(test, "bl:mp error");
            if (test->options.verbosity >= VERB_FAILURES) {
                Blimp_DumpLastError(test->blimp, stdout);
            }
            goto cleanup_parsed;
        }
    }

    clock_gettime(CLOCK_MONOTONIC, &end);
    size_t blimp_ns = (end  .tv_sec*1000000000 + end  .tv_nsec) -
                      (start.tv_sec*1000000000 + start.tv_nsec);
    PassTest(test, racket_ns/1000000, blimp_ns/1000000);

    if (result) BlimpObject_Release(result);
cleanup_parsed:
    Blimp_FreeExpr(expr);
cleanup:
    Blimp_Delete(test->blimp);
}


static void RunGroup(Group *group)
{
    memset(group->results, 0, sizeof(group->results));

    if (group->options.verbosity >= VERB_FAILURES) {
        printf(ANSI_PURPLE "Running tests for group" ANSI_RESET " %s\n",
            group->name);
    }

    for (size_t i = 0; i < group->num_tests; ++i) {
        RunTest(group->tests[i]);
        ++group->results[group->tests[i]->result];
    }

    if (group->options.verbosity >= VERB_GROUP) {
        PrintResults(ANSI_PURPLE "Results for" ANSI_RESET " %s",
            group->results, group->num_tests, group->name);
    }
    if (group->options.verbosity >= VERB_FAILURES) {
        printf("\n");
    }
}

static bool RunSuite(Suite *suite)
{
    size_t results[NUM_RESULT_TYPES] = {0};
    size_t num_tests = 0;
    for (size_t i = 0; i < suite->num_groups; ++i) {
        RunGroup(suite->groups[i]);
        for (TestResult result = 0; result < NUM_RESULT_TYPES; ++result) {
            results[result] += suite->groups[i]->results[result];
        }
        num_tests += suite->groups[i]->num_tests;
    }

    if (suite->options.verbosity >= VERB_SUITE) {
        PrintResults(ANSI_PURPLE "Total results" ANSI_RESET, results, num_tests);
    }
    return results[TEST_FAILED] == 0;
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

static void PrintUsage(FILE *f, int argc, char **argv)
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
    fprintf(f, "    --skip-blimp\n");
    fprintf(f, "        Do not run bl:mp evaluation tests.\n");
    fprintf(f, "\n");
    fprintf(f, "    --blimp-timeout SECONDS\n");
    fprintf(f, "        Consider a test failed if it takes more than SECONDS to evaluate\n");
    fprintf(f, "        in the bl:mp interpreter. SECONDS may be an integer or floating\n");
    fprintf(f, "        point literal. The default is 5.\n");
    fprintf(f, "\n");
    fprintf(f, "    --racket-timeout SECONDS\n");
    fprintf(f, "        Consider a test failed if it takes more than SECONDS to evaluate\n");
    fprintf(f, "        in the Redex semantic model. SECONDS may be an integer or floating\n");
    fprintf(f, "        point literal. The default is 5.\n");
    fprintf(f, "\n");
    fprintf(f, "    --perf-factor FACTOR\n");
    fprintf(f, "        Multiply timeouts by FACTOR. For example, if a test has a blimp-timeout\n");
    fprintf(f, "        of 1s, but perf-factor is 1.5, then the test will only fail if it takes\n");
    fprintf(f, "        more than 1.5 seconds to execute.\n");
    fprintf(f, "\n");
    fprintf(f, "        This can be used to account for variations in the underlying performance\n");
    fprintf(f, "        of the system where the tests are running. For example, if the test\n");
    fprintf(f, "        timeouts were calibrated on a 2GHz machine, but you want to run them on\n");
    fprintf(f, "        a 1GHz machine, you might use --perf-factor=2. Or, if you want to run\n");
    fprintf(f, "        the tests in a Debug configuration when they were calibrated for an\n");
    fprintf(f, "        optimized configuration, you can use an appropriate perf-factor.\n");
    fprintf(f, "\n");
    fprintf(f, "        The recommended way to use this is to determine the appropriate\n");
    fprintf(f, "        perf-factor for your setup (hardware and build configuration) using a\n");
    fprintf(f, "        known-good bl:mp build. Then persist this factor using\n");
    fprintf(f, "            cmake -DTEST_PERF_FACTOR=whatever ..\n");
    fprintf(f, "        in the build directory. Repeat for each build configuration.\n");
    fprintf(f, "\n");
    fprintf(f, "        All tests should be calibrated against the same perf-factor. If you\n");
    fprintf(f, "        needed to set a perf-factor for your setup, you should take this into\n");
    fprintf(f, "        account when annotating a new benchmark with --blimp-timeout or when\n");
    fprintf(f, "        updating an existing one. For example, if your perf-factor is 1.5 and\n");
    fprintf(f, "        a benchmark runs in 3s with your setup, you should annotate it with\n");
    fprintf(f, "        --blimp-timeout=2.\n");
    fprintf(f, "\n");
    fprintf(f, "        FACTOR may be a positive integer or floating point literal. The default\n");
    fprintf(f, "        is 1, unless it is overridden by setting the CMake variable\n");
    fprintf(f, "        TEST_PERF_FACTOR.\n");
    fprintf(f, "\n");
    fprintf(f, "    -v, --verbose [LEVEL]\n");
    fprintf(f, "        Show verbose output at LEVEL. LEVEL may be one of the following\n");
    fprintf(f, "        (each named verbosity level implies the level below it):\n");
    fprintf(f, "         * debug: show output useful for debugging the test runner\n");
    fprintf(f, "         * stats: show detailed statistics during benchmark tests\n");
    fprintf(f, "         * test: show output for each test run\n");
    fprintf(f, "         * failures: show output only for tests which fail\n");
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
    FLAG_SKIP_BLIMP,
    FLAG_RACKET_TIMEOUT,
    FLAG_BLIMP_TIMEOUT,
    FLAG_PERF_FACTOR,
} Flag;

const Options default_options = {
    .verbosity      = VERB_GROUP,
    .filter         = "",
    .use_racket     = true,
    .use_blimp      = true,
    .racket_timeout = 5000,
    .blimp_timeout  = 5000,
    .perf_factor    = DEFAULT_PERF_FACTOR,
        // This default is defined by CMake, so that it can be overridden
        // for each build configuration.
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
static bool ParseOptions(int argc, char **argv, Options *options, int *status)
{
    // If `status` is NULL, point it somewhere so we don't have to keep checking
    // for NULL.
    int dummy_status = 0;
    if (status == NULL) {
        status = &dummy_status;
    }

    struct option cli_options[] = {
        {"filter",         required_argument, NULL, FLAG_FILTER },
        {"skip-racket",    no_argument,       NULL, FLAG_SKIP_RACKET },
        {"skip-blimp",     no_argument,       NULL, FLAG_SKIP_BLIMP },
        {"racket-timeout", required_argument, NULL, FLAG_RACKET_TIMEOUT },
        {"blimp-timeout",  required_argument, NULL, FLAG_BLIMP_TIMEOUT },
        {"perf-factor",    required_argument, NULL, FLAG_PERF_FACTOR },
        {"verbose",        optional_argument, NULL, FLAG_VERBOSE },
        {"help",           no_argument,       NULL, FLAG_HELP },
        {0, 0, 0, 0},
    };

    optind = 1;
        // Since ParseOptions may be called more than once (for example, to
        // parse test-specific options) we need to reset this global variable
        // each time.
    int option, i = 1;
    while ((option = getopt_long(argc, argv, "f:v::h", cli_options, &i)) != -1) {
        switch (option) {
            case FLAG_FILTER:
                options->filter = optarg;
                break;

            case FLAG_SKIP_RACKET:
                options->use_racket = false;
                break;

            case FLAG_SKIP_BLIMP:
                options->use_blimp = false;
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

            case FLAG_BLIMP_TIMEOUT: {
                char *invalid;
                float seconds = strtof(optarg, &invalid);
                if (*invalid) {
                    fprintf(stderr,
                        "blimp-timeout: argument must be a number\n");
                    PrintUsage(stderr, argc, argv);
                    *status = EXIT_FAILURE;
                    return true;
                }
                options->blimp_timeout = seconds*1000;

                break;
            }

            case FLAG_PERF_FACTOR: {
                char *invalid;
                float factor = strtof(optarg, &invalid);
                if (*invalid) {
                    fprintf(stderr, "perf-factor: argument must be a number\n");
                    PrintUsage(stderr, argc, argv);
                    *status = EXIT_FAILURE;
                    return true;
                }
                options->perf_factor = factor;

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
                } else if (strcmp(optarg, "stats") == 0) {
                    options->verbosity = VERB_STATS;
                } else if (strcmp(optarg, "test") == 0) {
                    options->verbosity = VERB_TEST;
                } else if (strcmp(optarg, "failures") == 0) {
                    options->verbosity = VERB_FAILURES;
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
    suite->num_groups = 0;
    size_t groups_capacity = 0;

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

        size_t tests_capacity = 0;
        Group *group = malloc(sizeof(Group));
        group->name = strdup(group_de->d_name);
        group->tests = NULL;
        group->num_tests = 0;
        group->options = suite->options;

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

            // It is! Create a test for it.
            Test *test = malloc(sizeof(Test));
            test->name = strdup(test_de->d_name);
            test->options = group->options;
            test->blimp = TestBlimp_New(&test->options);
            FILE *test_file = fdopen(
                openat(dirfd(group_dir), test_de->d_name, O_RDONLY), "r");
            Blimp_Check(Blimp_OpenFileStream(
                test->blimp, test_de->d_name, test_file, &test->stream));
            test->racket = &suite->racket;

            // Override group options with test-specific options.
            char *first_line = NULL;
            size_t n = 0;
            ssize_t line_len = getline(&first_line, &n, test_file);
            if (first_line && first_line[0] == '#' && first_line[1] == ':') {
                // The first line of the file is an options string.

                // Strip trailing newline.
                if (first_line[line_len - 1] == '\n') {
                    first_line[line_len - 1] = '\0';
                }

                // Split into words, using the same string splitting altgorithm
                // as the shell to respect quoted words with whitespace.
                wordexp_t split = { .we_offs = 1 };
                wordexp(&first_line[2], &split, WRDE_NOCMD|WRDE_DOOFFS);
                    // Flags:
                    //  WRDE_NOCMD: don't do command substitution.
                    //  WRDE_DOOFFS:
                    //      insert an initial NULL before the split words in the
                    //      resulting `we_wordv` array. We will replace this
                    //      with a pointer to the name of the executable
                    //      (blimp-test), since ParseOptions expects this to be
                    //      the first argument.

                // Prepend the name of the executable.
                split.we_wordv[0] = "blimp-test";
                split.we_wordc++;

                // Parse the array of split words as options.
                ParseOptions(
                    split.we_wordc, split.we_wordv, &test->options, NULL);

                // Normally, after calling wordexp, we would call wordfree to
                // free the memory that wordexp allocated. In this case, though,
                // we want that memory to persist, since the Options structure
                // might contain pointers to strings allocated by wordexp.
                //
                // We can, however, get rid of the raw line that we read from
                // file.
                free(first_line);
            }
            rewind(test_file);

            // Add it to our list of tests.
            if (group->num_tests >= tests_capacity) {
                tests_capacity = tests_capacity*2 + 1;
                group->tests = realloc(
                    group->tests, sizeof(Test *)*tests_capacity);
            }
            group->tests[group->num_tests++] = test;
        }

        // If we found any tests in this directory, create a group for them.
        if (group->tests) {
            // Sort the tests alphabetically.
            qsort(group->tests, group->num_tests, sizeof(Test *), CompareTests);

            // Add the group to the suite.
            if (suite->num_groups >= groups_capacity) {
                groups_capacity = groups_capacity*2 + 1;
                suite->groups = realloc(
                    suite->groups, sizeof(Group *)*groups_capacity);
            }
            suite->groups[suite->num_groups++] = group;
        } else {
            // Ditch the empty group.
            free(group);
        }

        closedir(group_dir);
    }

    closedir(suite_dir);

    return suite;
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
