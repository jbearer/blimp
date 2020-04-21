#include <stdarg.h>
#include <stdio.h>
#include <sys/types.h>

#include "internal/error.h"

#define ANSI_GREEN  "\e[1;32m"
#define ANSI_RED    "\e[1;31m"
#define ANSI_PURPLE "\e[1;35m"
#define ANSI_RESET  "\e[0m"

static ssize_t Getline(char **line, size_t *length, FILE *f)
{
    if (*line == NULL) {
        // Initialize buffer.
        *length = 80;
        *line = malloc(*length);
        if (*line == NULL) {
            return -1;
        }
    }

    // Read characters and append them to the buffer until we hit EOF or '\n'.
    size_t row = 0;
    int c;
    while ((c = fgetc(f)) != EOF) {
        if (row >= *length) {
            *length *= 2;
            *line = realloc(*line, *length);
            if (*line == NULL) {
                return -1;
            }
        }
        (*line)[row++] = c;
        if (c == '\n') {
            break;
        }
    }

    // Append a terminating byte.
    if (row >= *length) {
        *length += 1;
        *line = realloc(*line, *length);
    }
    (*line)[row] = '\0';

    return row;
}

static void PrintSourceRange(FILE *f, const SourceRange *range)
{
    FILE *source_file;

    // Print the location of the error.
    if (
        range->start.file && range->end.file &&
        strcmp(range->start.file, range->end.file) == 0
    ) {
        // If start and end are in the same file...
        if (range->start.row == range->end.row) {
            // If they are on the same line...
            if (range->start.col == range->end.col) {
                // If they are at the same column, print
                //  file:row:col
                fprintf(f, "%s%s%s:%zu:%zu\n",
                    ANSI_PURPLE, range->start.file, ANSI_RESET,
                    range->start.row+1, range->start.col+1);
            } else {
                // Otherwise, print
                //  file: row:col-col
                fprintf(f, "%s%s%s: %zu:%zu-%zu\n",
                    ANSI_PURPLE, range->start.file, ANSI_RESET,
                    range->start.row+1, range->start.col+1, range->end.col+1);
            }
        } else {
            // Otherwise, print
            //  file: row:col - row:col
            fprintf(f, "%s%s%s: %zu:%zu - %zu:%zu\n",
                ANSI_PURPLE, range->start.file, ANSI_RESET,
                range->start.row+1, range->start.col+1,
                range->end.row+1, range->end.col+1);
        }
    } else {
        // Otherwise, print the full reference for start and end:
        //  file:row:col - file:row:col
        fprintf(f, "%s%s%s:%zu:%zu - %s%s%s:%zu:%zu\n",
            ANSI_PURPLE, range->start.file ? range->start.file : "", ANSI_RESET,
            range->start.row+1, range->start.col+1,
            ANSI_PURPLE, range->end.file ? range->end.file : "", ANSI_RESET,
            range->end.row+1, range->end.col+1);
    }

    // If the range makes sense, we also try to print the relevant source code.
    if (
        // Start and end must be in the same file.
        range->start.file && range->end.file &&
        strcmp(range->start.file, range->end.file) == 0 &&

        // Start must be before end.
        (range->start.row < range->end.row ||
         (range->start.row == range->end.row &&
          range->start.col <= range->end.col)) &&

        // We must be able to open the file for reading.
        (source_file = fopen(range->start.file, "r")) != NULL
    ) {
        char *line = NULL;
        size_t length = 0;
        size_t row = 0;

        // Skip lines until we get to the source location.
        for (; row < range->start.row; ++row) {
            int ret = Getline(&line, &length, source_file);
            free(line);
            line = NULL;
            length = 0;

            if (ret < 0) {
                fclose(source_file);
                return;
            }
        }

        bool one_line = range->start.row == range->end.row;

        // Echo lines from the source range to the output.
        for (; row <= range->end.row; ++row) {
            if (Getline(&line, &length, source_file) > 0) {
                if (one_line) {
                    fprintf(f, "    %s", line);
                } else {
                    // Print line numbers if the range is more than one line.
                    fprintf(f, "%5zu | %s", row, line);
                }
            }
            free(line);
            line = NULL;
            length = 0;
        }

        // If the start and end are on the same line, underline the error.
        if (one_line) {
            size_t col = 0;

            fputs(ANSI_GREEN, f);
            for (; col < 4+range->start.col; ++col) {
                fputc(' ', f);
            }
            for (; col <= 4+range->end.col; ++col) {
                fputc('~', f);
            }
            fputs(ANSI_RESET, f);
            fputc('\n', f);
        }

        fclose(source_file);
    }
}

static Status VError(
    Blimp *blimp,
    const SourceRange *range,
    BlimpErrorCode code,
    const char *fmt,
    va_list args)
{
    blimp->last_error.code = code;
    if (range) {
        blimp->last_error.range = *range;
        blimp->last_error.has_range = true;
    } else {
        blimp->last_error.has_range = false;
    }
    if (fmt) {
        vsnprintf(blimp->last_error.message, ERR_MSG_LEN, fmt, args);
    } else {
        blimp->last_error.message[0] = '\0';
    }

    return &blimp->last_error;
}

Status Blimp_Error(Blimp *blimp, BlimpErrorCode code)
{
    return Blimp_ErrorMsg(blimp, code, NULL);
}

Status Blimp_ErrorMsg(Blimp *blimp, BlimpErrorCode code, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    VError(blimp, NULL, code, fmt, args);
    va_end(args);

    return &blimp->last_error;
}

Status Blimp_ErrorAt(
    Blimp *blimp, SourceLoc loc, BlimpErrorCode code, const char *fmt, ...)
{
    SourceRange range = { loc, loc };

    va_list args;
    va_start(args, fmt);
    VError(blimp, &range, code, fmt, args);
    va_end(args);

    return &blimp->last_error;
}

Status Blimp_ErrorFrom(
    Blimp *blimp, SourceRange range, BlimpErrorCode code, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    VError(blimp, &range, code, fmt, args);
    va_end(args);

    return &blimp->last_error;
}

Status Blimp_Reraise(Blimp *blimp)
{
    return &blimp->last_error;
}

BlimpErrorCode Blimp_GetLastError(
    Blimp *blimp, const SourceRange **range, const char **message)
{
    if (range) {
        if (blimp->last_error.has_range) {
            *range = &blimp->last_error.range;
        } else {
            *range = NULL;
        }
    }
    if (message) {
        *message = blimp->last_error.message;
    }

    return blimp->last_error.code;
}

void Blimp_Check(Status err)
{
    if (err == BLIMP_OK) {
        return;
    }

    fprintf(stderr, "%sbl:mp error%s %zu%s%s\n",
        ANSI_RED, ANSI_RESET, (size_t)err->code,
        err->message[0] ? ": " : "",  err->message);

    if (err->has_range) {
        PrintSourceRange(stderr, &err->range);
    }

    exit(EXIT_FAILURE);
}
