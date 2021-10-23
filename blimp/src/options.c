#include <string.h>

#include "options.h"

void DefaultOptions(Options *options)
{
    options->blimp_options = DEFAULT_BLIMP_OPTIONS;
    options->action        = ACTION_DEFAULT;
    options->interactive   = false;

    // By default, we look for modules in the current working directory, the
    // directory containing the standard extension modules (e.g. `system`), and
    // the directory contiaining the standard prelude.
    options->import_path_len = 3;
    options->import_path     = calloc(
        options->import_path_len + 1, sizeof(char *));
    options->import_path[0]  = ".";
    options->import_path[1]  = PRELUDE_PATH;
    options->import_path[2]  = EXTENSIONS_PATH;

    options->prepend_len = 0;
    options->prepend = NULL;

    const char *home = getenv("HOME");
    char *history_file = NULL;
    if (home) {
        history_file = calloc(
            strlen(home) + strlen("/.blimp_history") + 1, 1);
        if (history_file) {
            strcat(history_file, home);
            strcat(history_file, "/.blimp_history");
        }
    }
    options->history_file = history_file;
    options->history_limit = 1000;
    options->no_history_limit = false;

    options->dialect = DIALECT_STD;
    options->non_terminal = NULL;
    options->debug = false;
}

BlimpStatus Options_NonTerminal(
    const Options *options, Blimp *blimp, const BlimpSymbol **nt)
{
    // Figure out how to parse input based on the dialect.
    if (options->non_terminal == NULL) {
        switch (options->dialect) {
            case DIALECT_STD:
            case DIALECT_CORE: {
                return Blimp_GetSymbol(blimp, "1", nt);
            }
            default:
                return Blimp_GetSymbol(blimp, "_1", nt);
        }
    } else {
        return Blimp_GetSymbol(blimp, options->non_terminal, nt);
    }
}
