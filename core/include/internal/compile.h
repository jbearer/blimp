#ifndef BLIMP_COMPILE_H
#define BLIMP_COMPILE_H

#include "internal/common.h"
#include "internal/object.h"

/**
 * \brief Optimize a compiled bytecode routine.
 *
 * \param code
 *      The bytecode to optimize.
 * \param scope
 *      A scope containing the object whose code is being optimized. This is
 *      required, but need not be the object itself (the object may not even
 *      exist yet when the code is optimized!); it can be any object which is
 *      guaranteed to be a parent or ancestor of the eventual object itself,
 *      including the global object.
 * \param depth
 *      The lexical depth from `scope` to the actual object whose code is being
 *      optimized. This should be 0 if the code to be optimized belongs to
 *      `scope` itself, 1 if the code belongs to a direct child of `scope`, and
 *      so on.
 * \param[out] optimized
 *      A new bytecode routine which is equivalent to `code` when it executes in
 *      `scope.`
 */
PRIVATE Status Optimize(
    Bytecode *code, ScopedObject *scope, size_t depth, Bytecode **optimized);

/**
 * \brief
 *      Optimize a compiled bytecode routine for execution in a specific scope.
 *
 * \param code  The bytecode to optimize.
 * \param scope The scope in which the optimized code will run.
 * \param replace_subroutine
 *      If non-NULL, any direct references to `replace_subroutine` in `code`
 *      (e.g. BLOCKI instructions) will be replaced with an optimized version of
 *      the subroutine.
 * \param optimized_subroutine
 *      An optimized version of `replace_subroutine`, which will replace
 *      reference to `replace_subroutine` in `code`. Must be non-NULL whenever
 *      `replace_subroutine` is non-NULL.
 * \param specialized
 *      If `optimized_subroutine` is provided, `specialized` should be the
 *      sequence number of the scope in which it is specialized.
 * \param[out] optimized
 *      A new bytecode routine which is equivalent to `code` when it executes in
 *      `scope.`
 */
PRIVATE Status OptimizeForScope(
    Bytecode *code,
    ScopedObject *scope,
    Bytecode *replace_subroutine,
    Bytecode *optimized_subroutine,
    size_t specialized,
    Bytecode **optimized);

#endif
