#include <setjmp.h>
#include <signal.h>

#include "interrupt.h"
#include "readline.h"

#ifdef HAVE_READLINE

#include <readline/history.h>
#include <readline/readline.h>

static void InterruptReadline(void *arg)
{
    siglongjmp(*(sigjmp_buf *)arg, 1);
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

static void ReplaceLastHistoryEntry(const char *line)
{
    replace_history_entry(history_length - 1, line, NULL);
}

void Readline_Init(const Options *options)
{
    SetHistoryLimit(options);
    ReadHistory(options);
}

char *Readline(const char *prompt)
{
    sigjmp_buf env;
    if (sigsetjmp(env, true)) {
        putchar('\n');
    }

    PushInterruptHandler(InterruptReadline, &env);
    char *line = readline(prompt);
    PopInterruptHandler();

    if (line && *line && !isspace(*line)) {
        add_history(line);
    }
    return line;
}

void Readline_SaveHistory(const Options *options)
{
    if (options->history_file) {
        append_history(history_max_entries, options->history_file);
        SetHistoryLimit(options);
    }
}

HIST_ENTRY *LastHistory(void)
{
    return history_get(history_length);
}

#else

static void ReplaceLastHistoryEntry(const char *line)
{
    (void)line;
}

void Readline_Init(const Options *options)
{
    (void)options;
}

char *Readline(const char *prompt)
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

void Readline_SaveHistory(const Options *options)
{
    (void)options;
}

HIST_ENTRY *LastHistory(void)
{
    return NULL;
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

Expr *Readline_ReadExpr(
    Blimp *blimp, const char *prompt, bool blank_line_repeats)
{
    char *line;

    // Read until we get a non-empty line or end-of-file.
    while ((line = Readline(prompt)) != NULL && !*line) {
        free(line);

        if (blank_line_repeats) {
            // If we are treating blank lines as a repeat of the last expression
            // entered, look up the last entry from the history.
            HIST_ENTRY *entry = LastHistory();
            if (entry != NULL) {
                if (entry->line && *entry->line) {
                    line = strdup(entry->line);
                    break;
                }
            }
        }
    }

    if (line == NULL) {
        // Return NULL to indicate end-of-file.
        return NULL;
    }

    // Start a new input, which for now consists solely of `line`. We may
    // append more to it later if this ends up being a multi-line input.
    char *input = NULL;
    Blimp_Check(AppendLine(blimp, &input, line));
    free(line);

    // Try to parse the input we have so far. This will tell us if the input
    // is a complete expression or not. If not, we will ask the user for
    // more lines of input.
    Expr *expr;
    while (Blimp_ParseString(blimp, input, &expr) != BLIMP_OK) {
        if (Blimp_GetLastErrorCode(blimp) == BLIMP_UNEXPECTED_EOF) {
            // If we were expecting more input, print a continuation prompt
            // and read another line.
            line = Readline("   ... ");
            if (line == NULL) {
                // Return NULL to indicate end-of-file.
                free(input);
                return NULL;
            } else if (!*line) {
                // If the user hits return twice (that is, enters a blank
                // line) we assume they really to enter their input as is,
                // even if it causes a parse error.
                free(line);
                free(input);
                Blimp_DumpLastError(blimp, stdout);

                // Start from scratch, trying to read a new expression.
                return Readline_ReadExpr(blimp, prompt, blank_line_repeats);
            }

            // Add the new line to the input.
            Blimp_Check(AppendLine(blimp, &input, line));
            free(line);

            // Remove the partial input that was missing a line from the
            // history, and replace it with the updated input that contains
            // the most recent line.
            ReplaceLastHistoryEntry(input);
        } else {
            free(input);
            Blimp_DumpLastError(blimp, stdout);

            // The user entered an expression, but it didn't parse, so we have
            // nothing to return. Start from scratch, asking for another
            // expression.
            return Readline_ReadExpr(blimp, prompt, blank_line_repeats);
        }
    }

    free(input);
    return expr;
}
