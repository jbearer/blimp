#ifndef BLIMP_ERROR_H
#define BLIMP_ERROR_H

#include <string.h>

#include "internal/common.h"

// Abbreviations for the Blimp_Error* functions.
#define Error(...) Blimp_Error(__VA_ARGS__)
#define RuntimeError(...) Blimp_RuntimeError(__VA_ARGS__)
#define ErrorMsg(...) Blimp_ErrorMsg(__VA_ARGS__)
#define RuntimeErrorMsg(...) Blimp_ErrorRuntimeMsg(__VA_ARGS__)
#define ErrorAt(...) Blimp_ErrorAt(__VA_ARGS__)
#define RuntimeErrorAt(...) Blimp_ErrorRuntimeAt(__VA_ARGS__)
#define ErrorFrom(...) Blimp_ErrorFrom(__VA_ARGS__)
#define RuntimeErrorFrom(...) Blimp_ErrorRuntimeFrom(__VA_ARGS__)
#define Reraise(...) Blimp_Reraise(__VA_ARGS__)
#define RuntimeReraise(...) Blimp_RuntimeReraise(__VA_ARGS__)

// If `e` evalutes to an error, return that error from the function invoking
// this macro.
#define TRY(e) \
    do { \
        Status s = (e);\
        if (s != BLIMP_OK) { \
            return s; \
        } \
    } while (0)

#define TRY_FROM(range, e) \
    do { \
        Status s = (e); \
        if (s != BLIMP_OK) { \
            s->range = range; \
            return s; \
        } \
    } while (0)

// Wrapper around malloc which returns a Status. The allocated pointer `ret` is
// an out-param of type `T **`.
#define Malloc(blimp, size, ret) ( \
    (*(void **)(ret) = malloc(size)) == NULL \
        ? Error(blimp, BLIMP_OUT_OF_MEMORY) \
        : BLIMP_OK \
    )

#define Calloc(blimp, n, size, ret) (\
    (*(void **)(ret) = calloc(n, size)) == NULL \
        ? Error(blimp, BLIMP_OUT_OF_MEMORY) \
        : BLIMP_OK \
    )

// Wrapper around realloc which returns a Status. The pointer `p` to be resized
// an in-out-param of type `T **`.
#define Realloc(blimp, size, p) ( \
    (*(void **)(p) = realloc(*p, size)) == NULL \
        ? Error(blimp, BLIMP_OUT_OF_MEMORY) \
        : BLIMP_OK \
    )

// Wrapper around strdup which returns a Status.
static inline Status Strndup(
    Blimp *blimp, const char *str, size_t length, char **ret)
{
    TRY(Malloc(blimp, length, ret));
    strncpy(*ret, str, length);
    (*ret)[length-1] = '\0';
    return BLIMP_OK;
}

static inline Status Strdup(Blimp *blimp, const char *str, char **ret)
{
    return Strndup(blimp, str, strlen(str) + 1, ret);
}

// Wrapper around free which stores NULL into the freed pointer.
#define Free(blimp, p) do { \
    (void)blimp; \
    free(*p); \
    *p = NULL; \
} while (0)

PRIVATE void PrintSourceRange(FILE *f, const SourceRange *range);

#endif
