#include <assert.h>
#include <getopt.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <blimp.h>
#include <blimp/module.h>

#include "command.h"
#include "debug.h"
#include "interrupt.h"
#include "options.h"
#include "readline.h"

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
    fprintf(f, "    -g, --debug\n");
    fprintf(f, "        Run with an attached debugger. This is the default mode\n");
    fprintf(f, "        in interactive sessions, but must be specified explicitly\n");
    fprintf(f, "        when running a script.\n");
    fprintf(f, "\n");
    fprintf(f, "    -a, --action\n");
    fprintf(f, "        Set the action to perform on the input program:\n");
    fprintf(f, "            * eval: evaluate the program and print the result\n");
    fprintf(f, "            * parse: pretty-print the parsed input expression\n");
    fprintf(f, "            * dump: dump a representation of the parsed input\n");
    fprintf(f, "                  in the formal semantics.\n");
    fprintf(f, "            * compile: print the compiled bytecode for an input\n");
    fprintf(f, "                  expression\n");
    fprintf(f, "        The default is `eval' for interactive mode, `parse' for\n");
    fprintf(f, "        non-interactive mode.\n");
    fprintf(f, "\n");
    fprintf(f, "    -I, --import-path DIR\n");
    fprintf(f, "        Search for imported modules in DIR. Multiple directories\n");
    fprintf(f, "        may be given by passing this option more than once.\n");
    fprintf(f, "\n");
    fprintf(f, "    -l, --preload MOD\n");
    fprintf(f, "        Prepend the given module to the input file. MOD will be\n");
    fprintf(f, "        searched in the import path, using the same search\n");
    fprintf(f, "        procedure as `import MOD'. More than one preload module\n");
    fprintf(f, "        may be given by passing this option more than once.\n");
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

static BlimpStatus PreloadStream(
    Blimp *blimp, const Options *options, BlimpStream **stream)
{
    *stream = NULL;
    for (size_t i = 0; i < options->prepend_len; ++i) {
        // Locate the preload module in the module search path.
        const char *preload_path;
        BlimpModuleType type = BLIMP_MODULE_TEXT;
        if (BlimpModule_Search(
                blimp,
                options->prepend[i],
                options->import_path,
                &type,
                &preload_path
            ) != BLIMP_OK)
        {
            if (*stream != NULL) {
                BlimpStream_Delete(*stream);
            }
            return Blimp_Reraise(blimp);
        }

        // Open a stream for the preload module
        BlimpStream *file_stream;
        if (Blimp_FileStream(blimp, preload_path, &file_stream)
                != BLIMP_OK)
        {
            free((void *)preload_path);
            if (*stream != NULL) {
                BlimpStream_Delete(*stream);
            }
            return Blimp_Reraise(blimp);
        }
        free((void *)preload_path);

        // Append the new preload stream to the end of the stream we've already
        // built up.
        if (*stream == NULL) {
            *stream = file_stream;
        } else {
            if (Blimp_ConcatStreams(blimp, *stream, file_stream, stream)
                    != BLIMP_OK)
            {
                BlimpStream_Delete(*stream);
                BlimpStream_Delete(file_stream);
                return Blimp_Reraise(blimp);
            }
        }
    }

    return BLIMP_OK;
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
    (void)blimp;
    (void)scope;
    (void)receiver;

    BlimpObject_Print(stdout, message);

    *result = BlimpObject_Borrow(receiver);
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

static BlimpStatus InterruptBlimp(
    Blimp *blimp, size_t signum, const Instruction *ip, void *arg)
{
    (void)signum;
    (void)ip;

    Debugger *db = (Debugger *)arg;
    if (db != NULL && Debugger_GetAttachedBlimp(db) == blimp) {
        return Debugger_Repl(db);
    } else {
        BlimpStackTrace *trace;
        Blimp_Check(Blimp_SaveStackTrace(blimp, &trace));
        BlimpStackTrace_Print(stdout, trace, 0);
        Blimp_FreeStackTrace(blimp, trace);
    }

    return Blimp_Error(blimp, BLIMP_INTERRUPTED);
}

static void InterruptAction(void *arg)
{
    Blimp *blimp = (Blimp *)arg;
    Blimp_RaiseSignal(blimp, BLIMP_INTERRUPT_SIGNAL);
}

static BlimpStatus DoAction(
    Blimp *blimp, BlimpExpr *expr, const Options *options, Debugger *db)
{
    BlimpStatus status = BLIMP_OK;

    Action action = options->action;
    if (action == ACTION_DEFAULT) {
        if (options->interactive) {
            // In interactive mode, each input expression is evaluated
            // automatically, as if it had been prefixed with !. Fall
            // through to the ACTION_EVAL case.
            action = ACTION_EVAL;
        } else {
            // In non-interactive mode, evaluation _is_ parsing, because the
            // strict semantics of bl:mp state that the behavior of a
            // program is simply the side-effects of parsing that program.
            // `expr` is already parsed, so we have nothing left to do.
            return BLIMP_OK;
        }
    }

    Blimp_HandleSignal(blimp, BLIMP_INTERRUPT_SIGNAL, InterruptBlimp, db);
    PushInterruptHandler(InterruptAction, blimp);

    switch (action) {
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

    PopInterruptHandler();
    return status;
}

static int ReplMain(Blimp *blimp, const Options *options)
{
    // Initialize the REPL.
    Readline_Init(options);
    PrintVersion(stdout);
    InitCommands(blimp);
    printf("Type ?help for help.\n");

    // Create an input stream for the parser which yields the contents of
    // each preload file.
    BlimpStream *stream;
    if (PreloadStream(blimp, options, &stream) != BLIMP_OK) {
        Blimp_DumpLastError(blimp, stderr);
        return 1;
    }

    if (stream != NULL) {
        // Execute the preloaded files.
        BlimpParseTree tree;
        if (Blimp_Parse(blimp, stream, &tree) != BLIMP_OK) {
            Blimp_DumpLastError(blimp, stderr);
            return 1;
        }
        BlimpExpr *expr;
        if (BlimpParseTree_Eval(blimp, &tree, &expr) != BLIMP_OK) {
            BlimpParseTree_Destroy(&tree);
            Blimp_DumpLastError(blimp, stderr);
            return 1;
        }
        BlimpParseTree_Destroy(&tree);
        Blimp_FreeExpr(expr);
    }

    Debugger db;
    Debugger_Init(&db);
    InitDebuggerCommands(blimp, &db);

    BlimpExpr *expr;
    while ((expr = Readline_ReadExpr(blimp, "bl:mp> ", false)) != NULL) {
        if (DoAction(blimp, expr, options, &db) != BLIMP_OK) {
            Blimp_DumpLastError(blimp, stdout);
        }
        Blimp_FreeExpr(expr);

        if (Debugger_GetAttachedBlimp(&db) != NULL) {
            // If the debugger is attached, send a `stepi` command so that it
            // drops into the debugger REPL at the start of the next expression.
            Debugger_StepI(&db);
        }
    }

    putchar('\n');
        // Write a newline so the user's terminal prompt doesn't appear on
        // the same line as the last bl:mp prompt.

    Debugger_Detach(&db);
    Readline_SaveHistory(options);
    return EXIT_SUCCESS;
}

static int EvalMain(Blimp *blimp, const char *path, const Options *options)
{
    // Create an input stream for the parser which first yields the contents of
    // each preload file and then the contents of `path`.
    BlimpStream *stream;
    if (PreloadStream(blimp, options, &stream) != BLIMP_OK) {
        Blimp_DumpLastError(blimp, stderr);
        return 1;
    }
    {
        // Open a stream for the input file.
        BlimpStream *file_stream;
        if (Blimp_FileStream(blimp, path, &file_stream) != BLIMP_OK) {
            if (stream != NULL) {
                BlimpStream_Delete(stream);
            }
            Blimp_DumpLastError(blimp, stderr);
            return 1;
        }

        // Append the stream to the end of the stream with the preload files.
        if (Blimp_ConcatStreams(blimp, stream, file_stream, &stream)
                != BLIMP_OK)
        {
            BlimpStream_Delete(stream);
            BlimpStream_Delete(file_stream);
            Blimp_DumpLastError(blimp, stderr);
            return 1;
        }
    }

    Debugger db;
    if (options->debug) {
        Debugger_Init(&db);
        Blimp_Check(Debugger_Attach(&db, blimp));

        Readline_Init(options);
        InitCommands(blimp);
        InitDebuggerCommands(blimp, &db);

        printf("The glorious bl:mp debugger, version %d.%d.%d\n",
            VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
        printf("Type ?help for help.\n");
    }

    // We have to prepare to be interrupted (and drop into the debugger if
    // necessary) during parsing, because macro handlers might cause code to be
    // executed at parse time.
    Blimp_HandleSignal(
        blimp,
        BLIMP_INTERRUPT_SIGNAL,
        InterruptBlimp,
        options->debug ? &db : NULL
    );
    PushInterruptHandler(InterruptAction, blimp);
    BlimpParseTree tree;
    if (Blimp_Parse(blimp, stream, &tree) != BLIMP_OK) {
        Blimp_DumpLastError(blimp, stderr);
        if (options->debug) {
            Debugger_Detach(&db);
        }
        return 1;
    }
    BlimpExpr *expr;
    if (BlimpParseTree_Eval(blimp, &tree, &expr) != BLIMP_OK) {
        BlimpParseTree_Destroy(&tree);
        Blimp_DumpLastError(blimp, stderr);
        if (options->debug) {
            Debugger_Detach(&db);
        }
        return 1;
    }
    BlimpParseTree_Destroy(&tree);
    PopInterruptHandler();

    int ret = EXIT_SUCCESS;
    if (DoAction(blimp, expr, options, options->debug ? &db : NULL)
            != BLIMP_OK)
    {
        Blimp_DumpLastError(blimp, stderr);
        ret = EXIT_FAILURE;
    }
    Blimp_FreeExpr(expr);

    if (options->debug) {
        Debugger_Detach(&db);
    }

    return ret;
}

typedef enum {
    FLAG_BLIMP_OPTION       = 'f',
    FLAG_OPTIMIZE           = 'O',
    FLAG_IMPORT_PATH        = 'I',
    FLAG_PRELOAD            = 'l',
    FLAG_DEBUG              = 'g',
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
        {"debug",                   no_argument,        NULL, FLAG_DEBUG},
        {"import-path",             required_argument,  NULL, FLAG_IMPORT_PATH},
        {"preload",                 required_argument,  NULL, FLAG_PRELOAD},
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
    while ((option = getopt_long(argc, argv, "gf:OI:l:a:hv", flags, &i)) != -1) {
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

            case FLAG_DEBUG:
                options.debug = true;
                break;

            case FLAG_IMPORT_PATH:
                options.import_path = realloc(
                    options.import_path,
                    ++options.import_path_len * sizeof(char *)
                );
                if (options.import_path == NULL) {
                    perror("could not allocate import path");
                    return EXIT_FAILURE;
                }
                options.import_path[options.import_path_len-2] = optarg;
                options.import_path[options.import_path_len-1]  = NULL;
                break;

            case FLAG_PRELOAD:
                options.prepend = realloc(
                    options.prepend,
                    ++options.prepend_len * sizeof(char *)
                );
                if (options.prepend == NULL) {
                    perror("could not allocate preload");
                    return EXIT_FAILURE;
                }
                options.prepend[options.prepend_len-1] = optarg;
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

    // Automatically prepend the `std_bootstrap' prelude if requested by the
    // user.
    if (options.implicit_prelude) {
        options.prepend = realloc(
            options.prepend, ++options.prepend_len * sizeof(char *));
        if (options.prepend == NULL) {
            fprintf(stderr, "could not allocate preload");
            return EXIT_FAILURE;
        }
        options.prepend[options.prepend_len-1] = "std_bootstrap";
    }

    if (optind == argc) {
        options.interactive = true;
        return ReplMain(blimp, &options);
    } else if (optind + 1 == argc) {
        options.interactive = false;
        return EvalMain(blimp, argv[optind], &options);
    } else {
        PrintUsage(stderr, argc, argv);
        return EXIT_FAILURE;
    }
}
