#include <string.h>

#include "options.h"

void DefaultOptions(Options *options)
{
    options->blimp_options = DEFAULT_BLIMP_OPTIONS;
    options->action        = ACTION_EVAL;

    options->import_path_len = 1;
    options->import_path     = calloc(
        options->import_path_len + 1, sizeof(char *));
    options->import_path[0]  = ".";

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
}
