#ifndef BLIMP_COMMON_H
#define BLIMP_COMMON_H

#include <string.h>

#include "blimp.h"

#define PRIVATE __attribute__((visibility("hidden")))

// Abbreviations for common types.
typedef BlimpStatus Status;
typedef BlimpSourceLoc SourceLoc;
typedef BlimpSourceRange SourceRange;
typedef BlimpStackTrace StackTrace;
typedef BlimpSymbol Symbol;
typedef BlimpStream Stream;
typedef BlimpExpr Expr;
typedef BlimpBytecode Bytecode;
typedef BlimpInstruction Instruction;
typedef BlimpObject Object;
typedef BlimpTerminal Terminal;
typedef BlimpNonTerminal NonTerminal;
typedef BlimpGrammarSymbol GrammarSymbol;
typedef BlimpParseTree ParseTree;
typedef BlimpParserContext ParserContext;

#define SubTree BlimpParseTree_SubTree

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

// If `e` evalutes to an error, return that error from the function invoking
// this macro.
#define TRY(e) \
    do { \
        Status s = (e);\
        if (s != BLIMP_OK) { \
            return s; \
        } \
    } while (0)

#define CHECK(e) \
    do { \
        Status s = (e); \
        assert(s == BLIMP_OK); \
    } while (0)

// Wrapper around malloc which returns a Status. The allocated pointer `ret` is
// an out-param of type `T **`.
#define Malloc(blimp, size, ret) ( \
    (*(void **)(ret) = malloc(size)) == NULL \
        ? Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY) \
        : BLIMP_OK \
    )

#define Calloc(blimp, n, size, ret) (\
    (*(void **)(ret) = calloc(n, size)) == NULL \
        ? Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY) \
        : BLIMP_OK \
    )

// Wrapper around realloc which returns a Status. The pointer `p` to be resized
// an in-out-param of type `T **`.
#define Realloc(blimp, size, p) ( \
    (*(void **)(p) = realloc(*p, size)) == NULL \
        ? Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY) \
        : BLIMP_OK \
    )

// Wrapper around strdup which returns a Status.
static inline Status Strndup(
    Blimp *blimp, const char *str, size_t length, char **ret)
{
    TRY(Malloc(blimp, length+1, ret));
    memcpy(*ret, str, length);
    (*ret)[length] = '\0';
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

#endif
