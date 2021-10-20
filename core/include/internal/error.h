#ifndef BLIMP_ERROR_H
#define BLIMP_ERROR_H

#include <string.h>

#include "common.h"

// Abbreviations for the Blimp_Error* functions.
#define Error(...) Blimp_Error(__VA_ARGS__)
#define RuntimeError(...) Blimp_RuntimeError(__VA_ARGS__)
#define ErrorMsg(...) Blimp_ErrorMsg(__VA_ARGS__)
#define RuntimeErrorMsg(...) Blimp_RuntimeErrorMsg(__VA_ARGS__)
#define ErrorAt(...) Blimp_ErrorAt(__VA_ARGS__)
#define RuntimeErrorAt(...) Blimp_RuntimeErrorAt(__VA_ARGS__)
#define ErrorFrom(...) Blimp_ErrorFrom(__VA_ARGS__)
#define RuntimeErrorFrom(...) Blimp_RuntimeErrorFrom(__VA_ARGS__)
#define ErrorFromExpr(...) Blimp_ErrorFromExpr(__VA_ARGS__)
#define RuntimeErrorFromExpr(...) Blimp_RuntimeErrorFromExpr(__VA_ARGS__)
#define Reraise(...) Blimp_Reraise(__VA_ARGS__)
#define RuntimeReraise(...) Blimp_RuntimeReraise(__VA_ARGS__)
#define ReraiseFrom(...) Blimp_ReraiseFrom(__VA_ARGS__)

#define TRY_FROM(RANGE, EXPR) TRY(AddRange(RANGE, EXPR))

PRIVATE Status ErrorFromOpt(
    Blimp *blimp,
    const SourceRange *range,
    BlimpErrorCode code,
    const char *fmt,
    ...);
PRIVATE Status ReraiseFromOpt(Blimp *blimp, const SourceRange *range);
PRIVATE void AppendErrorMsg(Blimp *blimp, const char *fmt, ...);
PRIVATE void PrintSourceRange(FILE *f, const SourceRange *range);
PRIVATE bool HasRange(Status status);
PRIVATE Status AddRange(const SourceRange *range, Status status);

#endif
