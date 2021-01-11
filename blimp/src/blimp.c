#include <assert.h>
#include <getopt.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <readline/history.h>
#include <readline/readline.h>

#include <blimp.h>
#include <blimp/module.h>

#include "command.h"
#include "interrupt.h"
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
    fprintf(f, "    -O\n");
    fprintf(f, "        Enable all compiler optimizations.\n");
    fprintf(f, "\n");
    fprintf(f, "    --core\n");
    fprintf(f, "        Use core bl:mp. This disables the implicit import of\n");
    fprintf(f, "        the `std' prelude.\n");
    fprintf(f, "\n");
    fprintf(f, "    -a, --action\n");
    fprintf(f, "        Set the action to perform on the input program:\n");
    fprintf(f, "            * eval: evaluate the program and print the result\n");
    fprintf(f, "            * parse: pretty-print the parsed input expression\n");
    fprintf(f, "            * dump: dump a representation of the parsed input\n");
    fprintf(f, "                  in the formal semantics.\n");
    fprintf(f, "            * compile: print the compiled bytecode for an input\n");
    fprintf(f, "                  expression\n");
    fprintf(f, "        The default is `eval'.\n");
    fprintf(f, "\n");
    fprintf(f, "    --history-file FILE\n");
    fprintf(f, "        Load and save interactive history to and from FILE.\n");
    fprintf(f, "        The default is `~/.blimp_history'.\n");
    fprintf(f, "\n");
    fprintf(f, "    --history-limit N\n");
    fprintf(f, "    --no-history-limit\n");
    fprintf(f, "        Set the maximum number of entries saved in the history\n");
    fprintf(f, "        buffer at a time. Setting this to something small can\n");
    fprintf(f, "        help speed up initialization times for the interactive\n");
    fprintf(f, "        REPL. The default is N=1000\n");
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

static bool IsRenderableStandardInstance(Blimp *blimp, BlimpObject *obj)
{
    // Try to determine if the object is a standard instance of a class which
    // responds to `render`. Classes which respond to methods have a symbol
    // matching the method name in their scope.
    const BlimpSymbol *render;
    if (Blimp_GetSymbol(blimp, "render", &render) != BLIMP_OK) {
        return false;
    }
    BlimpObject *obj_render;
    if (BlimpObject_Get(obj, render, &obj_render) != BLIMP_OK) {
        return false;
    }

    // Technically, we expect every object to have the symbol `render` in its
    // scope, since `render` is defined as a global function for symbols. What
    // we really care about is if the definition of `render` in the object's
    // scope is specific to that object; that is, it is different from the
    // global definition.
    BlimpObject *global_render;
    if (BlimpObject_Get(
            Blimp_GlobalObject(blimp), render, &global_render) != BLIMP_OK)
    {
        // If for some reason `render` is not in scope globally, yet it is in
        // scope for `obj`, then `obj` must have its own definition of `render`.
        return true;
    }
    if (obj_render == global_render) {
        return false;
    }

    // This object at least looks like it responds to render, so we will try to
    // fancy-render it.
    return true;
}

static bool IsRenderableWrapperInstance(Blimp *blimp, BlimpObject *obj)
{
    // Wrapper instances do not actually have their methods in their scope, but
    // they do capture a vtable which has the methods in its scope.
    const BlimpSymbol *vtable;
    if (Blimp_GetSymbol(blimp, "vtable", &vtable) != BLIMP_OK) {
        return false;
    }
    BlimpObject *obj_vtable;
    if (BlimpObject_GetCapturedMessageByName(
            obj, vtable, &obj_vtable) != BLIMP_OK)
    {
        return false;
    }

    // The vtable should quack like a renderable standard instance.
    return IsRenderableStandardInstance(blimp, obj_vtable);
}

static bool FancyRender(Blimp *blimp, BlimpObject *obj, const Options *options)
{
    if (!options->implicit_prelude) {
        // If --core is given, don't try to do anything fancy and format the
        // object, since the user is probably working with a lot of objects that
        // don't conform to the core library formatting protocol.
        return false;
    }

    // If the object is a symbol, just dumping the object will already produce
    // nicely formatted output.
    const BlimpSymbol *sym;
    if (BlimpObject_ParseSymbol(obj, &sym) == BLIMP_OK) {
        return false;
    }

    return IsRenderableStandardInstance(blimp, obj)
        || IsRenderableWrapperInstance(blimp, obj);
}

static BlimpStatus StdoutStream(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)scope;
    (void)receiver;

    BlimpObject_Print(stdout, message);

    *result = BlimpObject_Borrow(Blimp_GlobalObject(blimp));
    return BLIMP_OK;
}

static BlimpStatus Render(
    Blimp *blimp, BlimpObject *obj, const Options *options)
{
    if (obj == Blimp_GlobalObject(blimp)) {
        return BLIMP_OK;
    }

    if (FancyRender(blimp, obj, options)) {
        // Send the object a message to render itself to stdout.

        // Create the output stream.
        BlimpObject *out;
        if (BlimpObject_NewExtension(
                blimp,
                Blimp_GlobalObject(blimp),
                NULL,
                StdoutStream,
                NULL,
                &out)
            != BLIMP_OK)
        {
            return Blimp_Reraise(blimp);
        }

        // Create the `render` message.
        const BlimpSymbol *render;
        if (Blimp_GetSymbol(blimp, "render", &render) != BLIMP_OK) {
            BlimpObject_Release(out);
            return Blimp_Reraise(blimp);
        }
        BlimpObject *render_msg;
        if (BlimpObject_NewSymbol(blimp, render, &render_msg) != BLIMP_OK) {
            BlimpObject_Release(out);
            return Blimp_Reraise(blimp);
        }

        // Send the symbol `render` to `obj`, getting its implementation of
        // `render`.
        BlimpObject *render_handler;
        if (Blimp_Send(
                blimp,
                Blimp_GlobalObject(blimp),
                obj,
                render_msg,
                &render_handler)
            != BLIMP_OK)
        {
            BlimpObject_Release(out);
            BlimpObject_Release(render_msg);
            return Blimp_Reraise(blimp);
        }
        BlimpObject_Release(render_msg);

        // Send the output stream to the render handler.
        if (Blimp_Send(
                blimp,
                Blimp_GlobalObject(blimp),
                render_handler,
                out,
                NULL)
            != BLIMP_OK)
        {
            BlimpObject_Release(out);
            BlimpObject_Release(render_handler);
            return Blimp_Reraise(blimp);
        }

        BlimpObject_Release(out);
        BlimpObject_Release(render_handler);
    } else {
        BlimpObject_Print(stdout, obj);
    }

    putchar('\n');
    return BLIMP_OK;
}

#define BLIMP_INTERRUPT_SIGNAL 0

static BlimpStatus InterruptBlimp(Blimp *blimp, size_t signum, void *arg)
{
    (void)signum;
    (void)arg;

    BlimpStackTrace *trace;
    Blimp_Check(Blimp_SaveStackTrace(blimp, &trace));
    BlimpStackTrace_Print(stdout, trace, 0);
    Blimp_FreeStackTrace(blimp, trace);

    return Blimp_Error(blimp, BLIMP_INTERRUPTED);
}

static void InterruptAction(void *arg)
{
    Blimp *blimp = (Blimp *)arg;
    Blimp_RaiseSignal(blimp, BLIMP_INTERRUPT_SIGNAL);
}

static BlimpStatus DoAction(
    Blimp *blimp, BlimpExpr *expr, const Options *options)
{
    BlimpStatus status = BLIMP_OK;

    Blimp_HandleSignal(blimp, BLIMP_INTERRUPT_SIGNAL, InterruptBlimp, NULL);
    OnInterrupt(InterruptAction, blimp);

    switch (options->action) {
        case ACTION_EVAL: {
            BlimpObject *result;
            if ((status = Blimp_Eval(
                    blimp, expr, Blimp_GlobalObject(blimp), &result))
                != BLIMP_OK)
            {
                break;
            }

            if ((status = Render(blimp, result, options)) != BLIMP_OK) {
                break;
            }

            BlimpObject_Release(result);
            break;
        }

        case ACTION_PARSE: {
            Blimp_PrintExpr(blimp, stdout, expr);
            putchar('\n');
            break;
        }

        case ACTION_DUMP: {
            Blimp_DumpExpr(blimp, stdout, expr);
            putchar('\n');
            break;
        }

        case ACTION_COMPILE: {
            BlimpBytecode *code;
            if ((status = BlimpExpr_Compile(blimp, expr, &code)) != BLIMP_OK) {
                break;
            }

            BlimpBytecode_Print(stdout, code, true);
            BlimpBytecode_Free(code);
            break;
        }

        default: {
            assert(false);
            break;
        }
    }

    OnInterrupt(NULL, NULL);
    return status;
}

#ifdef HAVE_READLINE

#include <readline/readline.h>

static void InterruptReadline(void *arg)
{
    siglongjmp(*(sigjmp_buf *)arg, 1);
}

static char *Readline(const char *prompt)
{
    sigjmp_buf env;
    if (sigsetjmp(env, true)) {
        putchar('\n');
    }

    OnInterrupt(InterruptReadline, &env);
    char *line = readline(prompt);
    OnInterrupt(NULL, NULL);

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

static void SetHistoryLimit(const Options *options)
{
    if (options->no_history_limit) {
        unstifle_history();
    } else {
        stifle_history(options->history_limit);
        if (options->history_file) {
            history_truncate_file(
                options->history_file, options->history_limit);
        }
    }
}

static void WriteHistory(const Options *options)
{
    if (options->history_file) {
        append_history(history_max_entries, options->history_file);
        SetHistoryLimit(options);
    }
}

static void ReplaceLastHistoryEntry(const char *line)
{
    replace_history_entry(history_length - 1, line, NULL);
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

static void SetHistoryLimit(const Options *options)
{
    (void)options;
}

static void ReplaceLastHistoryEntry(const char *line)
{
    (void)line;
}

#endif

// Append a line to a string.
//
// If `*input` is `NULL`, then this function causes `*input` to point to a
// dynamically allocated string whose contents match those of `line`.
//
// Otherwise, `*input` must point to a dynamically allocated string created by a
// previous call to AppendLine. A newline character is appended to `*input`,
// followed by the contents of `line`, resizing `*input` if necessary.
//
// The memory pointed to by `input` must be freed by the caller, using free().
static BlimpStatus AppendLine(Blimp *blimp, char **input, const char *line)
{
    size_t input_len = 0;
    if (*input != NULL) {
        // Append a newline character to `*input`. This newline overwrites the
        // terminating null character. This is alright, since we've saved the
        // length of the string and we're about to append `line` to the end of
        // it, which will make it null-terminated wonce again.
        input_len = strlen(*input) + 1;
        (*input)[input_len - 1] = '\n';
    }

    // Resize the buffer to account for `line` and a trailing null character.
    *input = realloc(*input, input_len + strlen(line) + 1);
    if (*input == NULL) {
        return Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY);
    }

    // Append `line`.
    strcpy(*input + input_len, line);
    return BLIMP_OK;
}

static int ReplMain(Blimp *blimp, const Options *options)
{
    SetHistoryLimit(options);
    ReadHistory(options);
    PrintVersion(stdout);
    InitCommands(blimp);
    printf("Type ?help for help.\n");

    char *line;
    while ((line = Readline("bl:mp> ")) != NULL) {
        if (!*line) {
            free(line);
            goto err_empty_line;
        }

        // Start a new input, which for now consists solely of `line`. We may
        // append more to it later if this ends up being a multi-line input.
        char *input = NULL;
        Blimp_Check(AppendLine(blimp, &input, line));
        free(line);

        // Try to parse the input we have so far. This will tell us if the input
        // is a complete expression or not. If not, we will ask the users for
        // more lines of input.
        BlimpExpr *expr;
        while (Blimp_ParseString(blimp, input, &expr) != BLIMP_OK) {
            if (Blimp_GetLastErrorCode(blimp) == BLIMP_UNEXPECTED_EOF) {
                // If we were expecting more input, print a continuation prompt
                // and read another line.
                line = Readline("   ... ");
                if (line == NULL) {
                    free(input);
                    goto err_end_of_input;
                } else if (!*line) {
                    // If the user hits return twice (that is, enters a blank
                    // line) we assume they really to enter their input as is,
                    // even if it causes a parse error.
                    free(line);
                    Blimp_DumpLastError(blimp, stdout);
                    goto err_parse;
                }

                // Add the new line to the input.
                Blimp_Check(AppendLine(blimp, &input, line));
                free(line);

                // Remove the partial input that was missing a line from the
                // history, and replace it with the updated input that contains
                // the most recent line.
                ReplaceLastHistoryEntry(input);
            } else {
                Blimp_DumpLastError(blimp, stdout);
                goto err_parse;
            }
        }

        if (DoAction(blimp, expr, options) != BLIMP_OK) {
            Blimp_DumpLastError(blimp, stdout);
            goto err_action;
        }

err_action:
        Blimp_FreeExpr(expr);
err_parse:
        free(input);
err_empty_line:;
    }
err_end_of_input:

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
    if (DoAction(blimp, expr, options) != BLIMP_OK) {
        Blimp_DumpLastError(blimp, stderr);
        ret = EXIT_FAILURE;
    }
    Blimp_FreeExpr(expr);
    return ret;
}

typedef enum {
    FLAG_BLIMP_OPTION       = 'f',
    FLAG_OPTIMIZE           = 'O',
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
    FLAG_HISTORY_LIMIT,
    FLAG_NO_HISTORY_LIMIT,
} Flag;

int main(int argc, char *const *argv)
{
    struct option flags[] = {
        {"core",                    no_argument,        NULL, FLAG_CORE},
        {"import-path",             required_argument,  NULL, FLAG_IMPORT_PATH},
        {"action",                  required_argument,  NULL, FLAG_ACTION},
        {"history-file",            required_argument,  NULL, FLAG_HISTORY_FILE},
        {"history-limit",           required_argument,  NULL, FLAG_HISTORY_LIMIT},
        {"no-history-limit",        no_argument,        NULL, FLAG_NO_HISTORY_LIMIT},
        {"help",                    no_argument,        NULL, FLAG_HELP},
        {"version",                 no_argument,        NULL, FLAG_VERSION},
        {0, 0, 0, 0},
    };

    Options options;
    DefaultOptions(&options);

    int option, i = 1;
    while ((option = getopt_long(argc, argv, "f:OI:a:hv", flags, &i)) != -1) {
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

            case FLAG_OPTIMIZE:
                Blimp_OptimizationsOn(&options.blimp_options);
                break;

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
                } else if (strcmp(optarg, "parse") == 0) {
                    options.action = ACTION_PARSE;
                } else if (strcmp(optarg, "dump") == 0) {
                    options.action = ACTION_DUMP;
                } else if (strcmp(optarg, "compile") == 0) {
                    options.action = ACTION_COMPILE;
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

            case FLAG_HISTORY_LIMIT: {
                char *end;
                long n = strtol(optarg, &end, 0);
                if (*end || n < 0) {
                    fprintf(stderr,
                        "history-limit: argument must be a positive integer\n");
                    PrintUsage(stderr, argc, argv);
                    return EXIT_FAILURE;
                }

                options.history_limit = n;
                options.no_history_limit = false;
                break;
            }

            case FLAG_NO_HISTORY_LIMIT:
                options.no_history_limit = true;
                break;

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
