#ifndef BLIMP_OPTIONS_H
#define BLIMP_OPTIONS_H

#include <blimp.h>

typedef enum {
    ACTION_EVAL,
    ACTION_DUMP,
} Action;

typedef struct {
    BlimpOptions blimp_options;

    Action action;

    size_t import_path_len;
    const char **import_path;

    const char *history_file;
    size_t history_limit;
    bool no_history_limit;

    bool implicit_prelude;
} Options;

void DefaultOptions(Options *options);

#endif
