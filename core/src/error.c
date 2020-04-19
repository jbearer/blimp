#include <stdarg.h>

#include "internal/error.h"

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

    fprintf(stderr, "bl:mp: %s (%zu)\n", err->message, (size_t)err->code);
    exit(EXIT_FAILURE);
}
