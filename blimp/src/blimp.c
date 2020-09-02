#include <assert.h>
#include <getopt.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <readline/history.h>
#include <readline/readline.h>

#include <blimp.h>
#include <blimp/module.h>

#include "command.h"
#include "options.h"

#define VERSION_MAJOR 0
#define VERSION_MINOR 2
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
    fprintf(f, "    -f [no-]OPTION[=VALUE]\n");
    fprintf(f, "        Specify values for tunable interpreter properties. See\n");
    fprintf(f, "        below for a list of interpreter options.\n");
    fprintf(f, "\n");
    fprintf(f, "    --core\n");
    fprintf(f, "        Use core bl:mp. This disables the implicit import of\n");
    fprintf(f, "        the `std' prelude.\n");
    fprintf(f, "\n");
    fprintf(f, "    -a, --action\n");
    fprintf(f, "        Set the action to perform on the input program:\n");
    fprintf(f, "            * eval: evaluate the program and print the result\n");
    fprintf(f, "            * dump: dump the parsed input expression\n");
    fprintf(f, "        The default is `eval'.\n");
    fprintf(f, "\n");
    fprintf(f, "    --history-file FILE\n");
    fprintf(f, "        Load and save interactive history to and from FILE.\n");
    fprintf(f, "        The default is `~/.blimp_history'.\n");
    fprintf(f, "\n");
    fprintf(f, "    -h, --help\n");
    fprintf(f, "        Show this help and exit\n");
    fprintf(f, "\n");
    fprintf(f, "    -v, --version\n");
    fprintf(f, "        Print version information\n");
    fprintf(f, "\n");
    fprintf(f, "Interpreter options:\n");
    fprintf(f, "%s\n", BLIMP_OPTIONS_USAGE);
}

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

            if (result != Blimp_GlobalObject(blimp)) {
                BlimpObject_Print(stdout, result);
                putchar('\n');
            }

            BlimpObject_Release(result);
            return true;
        }

        case ACTION_DUMP: {
            Blimp_DumpExpr(blimp, stdout, expr);
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

static void ReadHistory(const Options *options)
{
    using_history();
    if (options->history_file) {
        read_history(options->history_file);
    }
}

static void WriteHistory(const Options *options)
{
    if (options->history_file) {
        append_history(history_length, options->history_file);
    }
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

static void ReadHistory(const Options *options)
{
    (void)options;
}

static void WriteHistory(const Options *options)
{
    (void)options;
}

#endif

static int ReplMain(Blimp *blimp, const Options *options)
{
    ReadHistory(options);
    PrintVersion(stdout);
    InitCommands(blimp);
    printf("Type ?help for help.\n");

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

        if (!DoAction(blimp, expr, options->action)) {
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

    WriteHistory(options);
    return EXIT_SUCCESS;
}

static int EvalMain(Blimp *blimp, const char *path, const Options *options)
{
    BlimpExpr *expr;
    if (Blimp_ParseFile(blimp, path, &expr) != BLIMP_OK) {
        Blimp_DumpLastError(blimp, stderr);
        return 1;
    }

    int ret = EXIT_SUCCESS;
    if (!DoAction(blimp, expr, options->action)) {
        Blimp_DumpLastError(blimp, stderr);
        ret = EXIT_FAILURE;
    }
    Blimp_FreeExpr(expr);
    return ret;
}

typedef enum {
    FLAG_BLIMP_OPTION       = 'f',
    FLAG_IMPORT_PATH        = 'I',
    FLAG_ACTION             = 'a',
    FLAG_HELP               = 'h',
    FLAG_VERSION            = 'v',

    FLAG_NO_SHORT_OPTION    = 'z'+1,
        // Dummy option which should be greater than all the short options
        // specified above. Any flag declared after this sentinel with no
        // explicit short option value will be automatically assigned a value
        // greater than the value of the sentinel, which should guarantee that
        // all the flags have unique values.

    FLAG_CORE,
    FLAG_HISTORY_FILE,
} Flag;

int main(int argc, char *const *argv)
{
    struct option flags[] = {
        {"core",                    no_argument,        NULL, FLAG_CORE},
        {"import-path",             required_argument,  NULL, FLAG_IMPORT_PATH},
        {"action",                  required_argument,  NULL, FLAG_ACTION},
        {"history-file",            required_argument,  NULL, FLAG_HISTORY_FILE},
        {"help",                    no_argument,        NULL, FLAG_HELP},
        {"version",                 no_argument,        NULL, FLAG_VERSION},
        {0, 0, 0, 0},
    };

    Options options;
    DefaultOptions(&options);

    int option, i = 1;
    while ((option = getopt_long(argc, argv, "f:I:a:hv", flags, &i)) != -1) {
        switch (option) {
            case FLAG_BLIMP_OPTION: {
                const char *error = Blimp_ParseOption(
                    optarg, &options.blimp_options);
                if (error) {
                    fprintf(stderr, "%s\n", error);
                    return EXIT_FAILURE;
                }

                break;
            }

            case FLAG_CORE:
                options.implicit_prelude = false;
                break;

            case FLAG_IMPORT_PATH:
                options.import_path = realloc(
                    options.import_path,
                    (options.import_path_len + 1)*sizeof(char *)
                );
                if (options.import_path == NULL) {
                    perror("could not allocate import path");
                    return EXIT_FAILURE;
                }
                options.import_path[options.import_path_len-1] = optarg;
                options.import_path[options.import_path_len]   = NULL;
                break;

            case FLAG_ACTION:
                if (strcmp(optarg, "eval") == 0) {
                    options.action = ACTION_EVAL;
                } else if (strcmp(optarg, "dump") == 0) {
                    options.action = ACTION_DUMP;
                } else {
                    fprintf(stderr, "action: invalid argument\n");
                    PrintUsage(stderr, argc, argv);
                    return EXIT_FAILURE;
                }
                break;

            case FLAG_HISTORY_FILE: {
                options.history_file = optarg;
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

    Blimp *blimp = Blimp_New(&options.blimp_options);
    if (blimp == NULL) {
        fprintf(stderr, "bl:mp: unable to initialize interpreter\n");
        return EXIT_FAILURE;
    }

    Blimp_Check(BlimpModule_Init(blimp, options.import_path));

    // Automatically import the `std' prelude if requested by the user.
    if (options.implicit_prelude) {
        BlimpObject *std;
        Blimp_Check(BlimpModule_Import(
            blimp,
            "std",
            Blimp_GlobalObject(blimp),
            options.import_path,
            &std
        ));
        BlimpObject_Release(std);
    }

    if (optind == argc) {
        return ReplMain(blimp, &options);
    } else if (optind + 1 == argc) {
        return EvalMain(blimp, argv[optind], &options);
    } else {
        PrintUsage(stderr, argc, argv);
        return EXIT_FAILURE;
    }
}
