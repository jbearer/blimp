#include <stdarg.h>
#include <stdio.h>
#include <sys/types.h>

#include "internal/blimp.h"
#include "internal/error.h"
#include "internal/expr.h"

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

void PrintSourceRange(FILE *f, const SourceRange *range)
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

        // The range must be a reasonable size: no more than 5 lines.
        (range->end.row - range->start.row <= 5) &&

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

static const char *GenericErrorCodeMessage(BlimpErrorCode code)
{
    switch (code) {
        case BLIMP_INVALID_CHARACTER: return "invalid character";
        case BLIMP_UNEXPECTED_TOKEN:  return "unexpected token";
        case BLIMP_UNEXPECTED_EOF:    return "unexpected end of input";
        case BLIMP_INVALID_MESSAGE_NAME: return "invalid message name or index";
        case BLIMP_AMBIGUOUS_PARSE:   return "ambiguous grammar rule";
        case BLIMP_INVALID_PARSE_TREE:return "invalid parse tree";
        case BLIMP_NO_SUCH_SYMBOL:    return "no such symbol";
        case BLIMP_NO_SUCH_METHOD:    return "no such method";
        case BLIMP_MUST_BE_BLOCK:     return "expected block";
        case BLIMP_MUST_BE_SYMBOL:    return "expected symbol";
        case BLIMP_STACK_OVERFLOW:    return "stack overflow";
        case BLIMP_INVALID_EXPR:      return "invalid expression";
        case BLIMP_OUT_OF_MEMORY:     return "out of memory";
        case BLIMP_IO_ERROR:          return "I/O error";
        case BLIMP_NOT_SUPPORTED:     return "operation not supported";
        case BLIMP_INTERRUPTED:       return "interrupted by user";
        default:                      return "unknown error";
    }
}

static Status VError(
    Blimp *blimp,
    const SourceRange *range,
    BlimpErrorCode code,
    bool runtime,
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

    if (blimp->last_error.trace != NULL) {
        Blimp_FreeStackTrace(blimp, blimp->last_error.trace);
    }
    if (runtime) {
        Blimp_SaveStackTrace(blimp, &blimp->last_error.trace);
    } else {
        blimp->last_error.trace = NULL;
    }

    if (fmt) {
        vsnprintf(blimp->last_error.message, ERR_MSG_LEN, fmt, args);
    } else {
        strncpy(
            blimp->last_error.message,
            GenericErrorCodeMessage(code),
            ERR_MSG_LEN
        );
    }

    return &blimp->last_error;
}

void AppendErrorMsg(Blimp *blimp, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    size_t curr_len = strlen(blimp->last_error.message);
    if (curr_len < ERR_MSG_LEN) {
        vsnprintf(
            blimp->last_error.message + curr_len, ERR_MSG_LEN - curr_len, fmt, args);
    }

    va_end(args);
}

Status Blimp_Error(Blimp *blimp, BlimpErrorCode code)
{
    return Blimp_ErrorMsg(blimp, code, NULL);
}

Status Blimp_RuntimeError(Blimp *blimp, BlimpErrorCode code)
{
    return Blimp_RuntimeErrorMsg(blimp, code, NULL);
}


Status Blimp_ErrorMsg(Blimp *blimp, BlimpErrorCode code, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    VError(blimp, NULL, code, false, fmt, args);
    va_end(args);

    return &blimp->last_error;
}

Status Blimp_RuntimeErrorMsg(
    Blimp *blimp, BlimpErrorCode code, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    VError(blimp, NULL, code, true, fmt, args);
    va_end(args);

    return &blimp->last_error;
}

Status Blimp_ErrorAt(
    Blimp *blimp, SourceLoc loc, BlimpErrorCode code, const char *fmt, ...)
{
    SourceRange range = { loc, loc };

    va_list args;
    va_start(args, fmt);
    VError(blimp, &range, code, false, fmt, args);
    va_end(args);

    return &blimp->last_error;
}

Status Blimp_RuntimeErrorAt(
    Blimp *blimp, SourceLoc loc, BlimpErrorCode code, const char *fmt, ...)
{
    SourceRange range = { loc, loc };

    va_list args;
    va_start(args, fmt);
    VError(blimp, &range, code, true, fmt, args);
    va_end(args);

    return &blimp->last_error;
}

Status Blimp_ErrorFrom(
    Blimp *blimp, SourceRange range, BlimpErrorCode code, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    VError(blimp, &range, code, false, fmt, args);
    va_end(args);

    return &blimp->last_error;
}

Status Blimp_RuntimeErrorFrom(
    Blimp *blimp, SourceRange range, BlimpErrorCode code, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    VError(blimp, &range, code, true, fmt, args);
    va_end(args);

    return &blimp->last_error;
}

Status Blimp_ErrorFromExpr(
    Blimp *blimp, Expr *expr, BlimpErrorCode code, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    VError(blimp, expr ? &expr->range : NULL, code, false, fmt, args);
    va_end(args);

    return &blimp->last_error;
}

Status Blimp_RuntimeErrorFromExpr(
    Blimp *blimp, Expr *expr, BlimpErrorCode code, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    VError(blimp, expr ? &expr->range : NULL, code, true, fmt, args);
    va_end(args);

    return &blimp->last_error;
}

Status Blimp_Reraise(Blimp *blimp)
{
    return &blimp->last_error;
}

Status Blimp_RuntimeReraise(Blimp *blimp)
{
    if (blimp->last_error.trace == NULL) {
        Blimp_SaveStackTrace(blimp, &blimp->last_error.trace);
    }

    return &blimp->last_error;
}

Status Blimp_ReraiseFrom(Blimp *blimp, SourceRange range)
{
    blimp->last_error.range = range;
    blimp->last_error.has_range = true;
    return &blimp->last_error;
}

BlimpErrorCode Blimp_GetLastError(
    Blimp *blimp,
    const SourceRange **range,
    StackTrace **trace,
    const char **message)
{
    if (range) {
        if (blimp->last_error.has_range) {
            *range = &blimp->last_error.range;
        } else {
            *range = NULL;
        }
    }

    if (trace) {
        *trace = NULL;
        if (blimp->last_error.trace) {
            Blimp_CopyStackTrace(blimp, blimp->last_error.trace, trace);
        }
    }

    if (message) {
        *message = blimp->last_error.message;
    }

    return blimp->last_error.code;
}

BlimpErrorCode Blimp_GetLastErrorCode(Blimp *blimp)
{
    return blimp->last_error.code;
}

static void DumpError(FILE *f, Status err, size_t trace_limit)
{
    if (err->trace) {
        fprintf(f, "Call stack (most recent call last):\n");
        BlimpStackTrace_Print(f, err->trace, trace_limit);
    }

    fprintf(f, "%sbl:mp error%s %zu%s%s\n",
        ANSI_RED, ANSI_RESET, (size_t)err->code,
        err->message[0] ? ": " : "",  err->message);

    if (err->has_range) {
        PrintSourceRange(f, &err->range);
    }
}

void Blimp_DumpLastError(Blimp *blimp, FILE *f)
{
    DumpError(f, &blimp->last_error, blimp->options.stack_trace_limit);
}

void Blimp_Check(Status err)
{
    if (err == BLIMP_OK) {
        return;
    }

    DumpError(stderr, err, 0);
    exit(EXIT_FAILURE);
}
