#ifndef BLIMP_COMMON_H
#define BLIMP_COMMON_H

#include "../blimp.h"

#define PRIVATE __attribute__((visibility("hidden")))

// Abbreviations for common types.
typedef BlimpStatus Status;
typedef BlimpSourceLoc SourceLoc;
typedef BlimpSourceRange SourceRange;
typedef BlimpStackTrace StackTrace;
typedef BlimpSymbol Symbol;
typedef BlimpStream Stream;
typedef BlimpExpr Expr;
typedef BlimpObject Object;
typedef BlimpMethod Method;

#ifdef NDEBUG
# ifdef assert
#  undef assert
# endif
# define assert(x) ((void)sizeof(x))
    // The default definition of `assert` in the release build doesn't evaluate
    // it's argument at all, which leads to annoying unused variable warnings
    // that don't show up in the debug build.
#else
# include <assert.h>
#endif

#endif
