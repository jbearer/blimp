#include <assert.h>
#include <getopt.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <readline/history.h>
#include <readline/readline.h>

#include "blimp.h"

#define VERSION_MAJOR 0
#define VERSION_MINOR 1
#define VERSION_PATCH 0

static void PrintVersion(FILE *f)
{
    fprintf(f, "The glorious bl:mp execution environment, version %d.%d.%d\n",
        VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
}

static void PrintUsage(FILE *f, int argc, char *const *argv)
{
    (void)argc;

    PrintVersion(f);
    fprintf(f, "\n");
    fprintf(f, "%s [options]\n", argv[0]);
    fprintf(f, "    Start an interactive bl:mp REPL.\n");
    fprintf(f, "%s [options] FILE\n", argv[0]);
    fprintf(f, "    Evaluate the bl:mp program contained in FILE.\n");
    fprintf(f, "\n");
    fprintf(f, "Options:\n");
    fprintf(f, "    -a, --action\n");
    fprintf(f, "        Set the action to perform on the input program:\n");
    fprintf(f, "            * eval: evaluate the program and print the result\n");
    fprintf(f, "            * dump: dump the parsed input expression\n");
    fprintf(f, "        The default is `eval'.\n");
    fprintf(f, "\n");
    fprintf(f, "    --object-pool-batch-size\n");
    fprintf(f, "\n");
    fprintf(f, "    -h, --help\n");
    fprintf(f, "        Show this help and exit\n");
    fprintf(f, "\n");
    fprintf(f, "    -v, --version\n");
    fprintf(f, "        Print version information\n");
}

typedef enum {
    ACTION_EVAL,
    ACTION_DUMP,
} Action;

static bool DoAction(Blimp *blimp, const BlimpExpr *expr, Action action)
{
    switch (action) {
        case ACTION_EVAL: {
            BlimpObject *result;
            if (Blimp_Eval(
                blimp, expr, Blimp_GlobalObject(blimp), &result) != BLIMP_OK)
            {
                return false;
            }

            BlimpObject_Print(stdout, result);
            putchar('\n');

            BlimpObject_Release(result);
            return true;
        }

        case ACTION_DUMP: {
            Blimp_DumpExpr(stdout, expr);
            putchar('\n');
            return true;
        }

        default: {
            assert(false);
            return false;
        }
    }
}

#ifdef HAVE_READLINE

#include <readline/readline.h>

static char *Readline(const char *prompt)
{
    char *line = readline(prompt);
    if (line && *line && !isspace(*line)) {
        add_history(line);
    }
    return line;
}

#else

static char *Readline(const char *prompt)
{
    puts(prompt);
    fflush(stdout);
        // We have to explicitly flush since we didn't write a newline.

    size_t length = 80;
    char *line = malloc(length);
    if (line == NULL) {
        return NULL;
    }

    // Read characters and append them to the buffer until we hit EOF or '\n'.
    size_t row = 0;
    int c;
    while ((c = getchar()) != EOF) {
        if (row >= length) {
            length *= 2;
            line = realloc(line, length);
            if (line == NULL) {
                return NULL;
            }
        }
        (*line)[row++] = c;
        if (c == '\n') {
            break;
        }
    }
    if (row == 0) {
        return NULL;
    }

    // Discard terminating newline.
    if (line[row-1] == '\n') {
        --row;
    }

    // Append a terminating byte.
    if (row >= length) {
        length += 1;
        line = realloc(line, length);
    }
    (line)[row] = '\0';

    return line;
}

#endif

static int ReplMain(Blimp *blimp, Action action)
{
#ifdef HAVE_READLINE
    using_history();
#endif

    PrintVersion(stdout);

    char *line;
    while ((line = Readline("bl:mp> ")) != NULL) {
        if (!*line) {
            goto err_empty_line;
        }

        BlimpExpr *expr;
        if (Blimp_ParseString(blimp, line, &expr) != BLIMP_OK) {
            Blimp_DumpLastError(blimp, stdout);
            goto err_parse;
        }

        if (!DoAction(blimp, expr, action)) {
            Blimp_DumpLastError(blimp, stdout);
            goto err_action;
        }

err_action:
        Blimp_FreeExpr(expr);
err_parse:
err_empty_line:
        free(line);
    }

    putchar('\n');
        // Write a newline so the user's terminal prompt doesn't appear on
        // the same line as the last bl:mp prompt.
    return EXIT_SUCCESS;
}

static int EvalMain(Blimp *blimp, Action action, const char *path)
{
    BlimpExpr *expr;
    if (Blimp_ParseFile(blimp, path, &expr) != BLIMP_OK) {
        Blimp_DumpLastError(blimp, stderr);
        return 1;
    }

    int ret = EXIT_SUCCESS;
    if (!DoAction(blimp, expr, action)) {
        Blimp_DumpLastError(blimp, stderr);
        ret = EXIT_FAILURE;
    }
    Blimp_FreeExpr(expr);
    return ret;
}

typedef enum {
    FLAG_ACTION             = 'a',
    FLAG_HELP               = 'h',
    FLAG_VERSION            = 'v',

    FLAG_NO_SHORT_OPTION    = 'z'+1,
        // Dummy option which should be greater than all the short options
        // specified above. Any flag declared after this sentinel with no
        // explicit short option value will be automatically assigned a value
        // greater than the value of the sentinel, which should guarantee that
        // all the flags have unique values.

    FLAG_OBJECT_POOL_BATCH_SIZE,
} Flag;

int main(int argc, char *const *argv)
{
    struct option options[] = {
        {"action",                  required_argument,  NULL, FLAG_ACTION},
        {"object-pool-batch-size",  required_argument,  NULL, FLAG_OBJECT_POOL_BATCH_SIZE},
        {"help",                    no_argument,        NULL, FLAG_HELP},
        {"version",                 no_argument,        NULL, FLAG_VERSION},
        {0, 0, 0, 0},
    };

    BlimpOptions blimp_options = DEFAULT_BLIMP_OPTIONS;
    Action action = ACTION_EVAL;

    int option, i = 1;
    while ((option = getopt_long(argc, argv, "hv", options, &i)) != -1) {
        switch (option) {
            case FLAG_ACTION:
                if (strcmp(optarg, "eval") == 0) {
                    action = ACTION_EVAL;
                } else if (strcmp(optarg, "dump") == 0) {
                    action = ACTION_DUMP;
                } else {
                    fprintf(stderr, "action: invalid argument\n");
                    PrintUsage(stderr, argc, argv);
                    return EXIT_FAILURE;
                }
                break;

            case FLAG_OBJECT_POOL_BATCH_SIZE: {
                char *invalid;
                blimp_options.object_pool_batch_size = strtol(
                    optarg, &invalid, 0);
                if (!*optarg || *invalid) {
                    fprintf(stderr,
                        "object-pool-batch-size: argument must be an integer\n");
                    PrintUsage(stderr, argc, argv);
                    return EXIT_FAILURE;
                }
                break;
            }

            case FLAG_HELP:
                PrintUsage(stdout, argc, argv);
                return EXIT_SUCCESS;

            case FLAG_VERSION:
                PrintVersion(stdout);
                return EXIT_SUCCESS;

            default:
                PrintUsage(stderr, argc, argv);
                return EXIT_FAILURE;
        }
    }

    Blimp *blimp = Blimp_New(&blimp_options);
    if (blimp == NULL) {
        fprintf(stderr, "bl:mp: unable to initialize interpreter\n");
        return EXIT_FAILURE;
    }

    if (i == argc) {
        return ReplMain(blimp, action);
    } else if (i + 1 == argc) {
        return EvalMain(blimp, action, argv[i]);
    } else {
        PrintUsage(stderr, argc, argv);
        return EXIT_FAILURE;
    }
}
