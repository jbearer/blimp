#ifndef BLIMP_OPTIONS_H
#define BLIMP_OPTIONS_H

#include <blimp.h>

typedef enum {
    DIALECT_RAW,
    DIALECT_CORE,
    DIALECT_STD,
} Dialect;

typedef enum {
    ACTION_DEFAULT,
    ACTION_EVAL,
    ACTION_PARSE,
    ACTION_DUMP,
    ACTION_COMPILE,
} Action;

typedef struct {
    BlimpOptions blimp_options;
    bool interactive;
    Action action;

    size_t import_path_len;
    const char **import_path;

    size_t prepend_len;
    const char **prepend;

    const char *history_file;
    size_t history_limit;
    bool no_history_limit;

    Dialect dialect;
    const char *non_terminal;
    bool debug;
} Options;

void DefaultOptions(Options *options);
BlimpStatus Options_NonTerminal(
    const Options *options, Blimp *blimp, const BlimpSymbol **nt);

#endif
