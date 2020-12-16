#ifndef BLIMP_H
#define BLIMP_H

#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * \defgroup interpreter Interpreter state
 *
 * Unlike some other embeddable interpreters, bl:mp does not store interpeter
 * state globally. Instead, all the state needed to run an interpreter is
 * encapsulated in the Blimp struct.
 *
 * @{
 */

/**
 * \struct Blimp
 * \copydoc interpreter
 */
typedef struct Blimp Blimp;

typedef struct {
    /**
     * \brief
     *      This option controls the maximum number of message sends allowed on
     *      the stack at a time.
     *
     * If `recursion_limit` is set to a nonzero number, then attempts to
     * evaluate a message send when there are already `recursion_limit` sends on
     * the stack will fail with a `BLIMP_STACK_OVERFLOW` error.
     *
     * If `recursion_limit` is 0, the limit is the size of the process stack.
     *
     * The default is 1000.
     */
    size_t recursion_limit;

    /**
     * \brief
     *      This option controls how much of large stack traces is printed when
     *      a runtime error occurs.
     *
     * If `stack_trace_limit` is set to a nonzero number, then when a stack
     * trace is printed, at most `stack_trace_limit` frames are printed from the
     * beginning of the trace, and at most `stack_trace_limit` frames are
     * printed from the end of the trace.
     *
     * Missing frames will be indicated with an ellipsis, as well as by a
     * discontinuity in the stack depth printed with each frame.
     *
     * If `stack_trace_limit` is zero, the entire stack trace will be printed.
     *
     * The default is 5.
     */
    size_t stack_trace_limit;

    /**
     * \brief
     *      When allocating new \ref objects, this option controls the number
     *      of objects allocated at once, to be initialized on demand later.
     *
     * Specifically, `gc_batch_size` is the number of bytes worth of
     * objects allocated at a time.
     *
     * Tuning this paramter adjusts the tradeoff between excess memory
     * usage (when `gc_batch_size` is larger than necessary) and high
     * allocation latency (when `gc_batch_size` is small). Since most
     * `bl:mp` programs allocate and free many short-lived objects
     * rapidly, overall performance is very sensitive to allocation
     * latency. If you're interested in tuning performance, you should try
     * to set this parameter as large as your memory constraints will
     * allow.
     *
     * The default is 1MB.
     */
    size_t gc_batch_size;

    /**
     * \brief
     *      This option controls whether automatic tracing garbage collection
     *      is enabled.
     *
     * Only tracing garbage collection can reliably free memory allocated to
     * reference cycles which are no longer reachable.
     *
     * If this option is set, the tracing garbage collector will run
     * periodically when a new object is needed and no previously freed objects
     * are available. `gc_batches_per_trace` can be used to configure how often
     * the system uses garbage collection to find free objects (which can be
     * very expensive) versus how often it allocates new batches when it needs a
     * free object (which is comparatively less expensive, but can result in
     * greater high water marks for memory usage).
     *
     * If this option is not set, the system will not attempt to free reference
     * cycles. It will continue allocating new batches of objects when free
     * objects are not available until the underlying allocator runs out of
     * memory.
     *
     * This option is enabled by default.
     */
    bool gc_tracing;

    /**
     * \brief
     *      This option controls how often the tracing garbage collector runs,
     *      if it is enabled.
     *
     * If this option is set to a positive value and `gc_tracing` is enabled,
     * we will allocate `gc_batches_per_trace` batches of objects between each
     * run of the tracing garbage collector. We will only run the collector if
     * we need a new object, there are no free objects available, and we have
     * allocated `gc_batches_per_trace` batches since the last GC sweep.
     * Otherwise, we will allocate a new batch without running the tracing GC.
     *
     * The default is 1.
     */
    size_t gc_batches_per_trace;

    /**
     * \brief
     *      This option controls whether internal reference counting is enabled.
     *
     * Reference counting for managed references is always enabled, because the
     * tracing garbage collector uses it to determine which objects the user of
     * the API is currently using, and which objects are reachable from those.
     *
     * This option affects reference counting for _internal_ references:
     * references between objects through their scopes and parents. If internal
     * reference counting is enabled, the system will sometimes be able to free
     * objects without invoking the tracing garbage collector by determining
     * that an object is not only unreferenced by the user, but it is also
     * unreferenced by any other object. This can improve performance in certain
     * applications.
     *
     * This option is enabled by default.
     */
    bool gc_refcount;

    /**
     * \brief
     *      This option controls whether enhanced reference counting is enabled.
     *
     * Standard internal reference counting (`gc_refcount`) is able to free
     * unreachable objects which are not referenced by other objects in a cycle,
     * without invoking the tracing garbage collector. Enhanced reference
     * counting extends this capability with limited cycle detection.
     *
     * Specifically, cycles formed by a single scope reference and one or more
     * parent references can be detected and collected without using the
     * tracing garbage collector.
     *
     * This allows the system to collect certain very common data structure
     * patterns using reference counting. For example, consider the case of a
     * simple local variable:
     *
     *      {
     *          local{^ value}
     *      }
     *
     * The outer block object clearly has a reference to `value` through `local`
     * in its scope. But `value` has a reference to the inner block object,
     * which is its parent. And that object has a reference to the outer block
     * object, which is _its_ parent, creating a cycle. `gc_refcount` on its own
     * would not be able to free any of the objects involved in this cycle; we
     * would need `gc_tracing` to do so. But with `gc_cycle_detection` enabled,
     * we will detect that these objects form a cycle, and we will free the
     * whole group of objects as soon as all references to objects in the cycle
     * originate from within the cycle; that is, as soon as there are no
     * references from outside the cycle to any of the objects in it.
     *
     * This option is enabled by default.
     */
    bool gc_cycle_detection;

    /**
     * \brief
     *      This option controls the maximum size of ERC clumps.
     *      It can also be used to prevent new objects from entangling with
     *      long-lived objects.
     *
     * If `gc_max_clump_size` is set to a nonzero value, then clumps larger than
     * `gc_max_clump_size` will stop entangling with new objects. Large clumps
     * can be expensive to manage, and each new object entangled with a clump
     * decreases the probability that that clump will ever be freed by ERC, so
     * limiting the clump size can sometimes be a performance optimization.
     *
     * In addition, objects created at different times, more than
     * `gc_max_clump_size` objects apart, are prohibited from entangling. This
     * means that if an object is particularly long-lived, once
     * `gc_max_clump_size` objects have been created after it, no new objects
     * will entangle with that object. This can prevent short-lived objects from
     * entangling with long-lived objects, which could prevent the short-lived
     * objects from being freed.
     *
     * The default is 0 (no limit on clump size or clump age difference).
     */
    size_t gc_max_clump_size;

    /**
     * \brief
     *      Enables runtime heap checking.
     *
     * If the bl:mp interpreter was built with debugging turned on and this
     * option is set, then the interpreter will periodically sweep the heap
     * looking for incosistencies and abort if it finds one. This can be used to
     * detect bugs in the built-in garbage collector.
     *
     * Enabling this option will significantly slow down execution.
     *
     * This option is disabled by default.
     */
    bool gc_heap_check;

    /**
     * \brief Enables tail call elimination.
     *
     * This option is enabled by default.
     */
    bool tail_call_elimination;


    /**
     * \brief Enables constant elision.
     *
     * Constant elision is an optimization which allows the interpreter to
     * detect when a symbol receiving a message is constant, and replace the
     * code with bytecode that sends the message directly to the value of the
     * symbol. This saves time by eliminating the need to look up the constant
     * value of the symbol each time that code executes. It also serves as a
     * gateway to further optimization, such as inlining.
     *
     * This option is disabled by default.
     */
    bool constant_elision;

    /**
     * \brief Enables inlining.
     *
     * When enabled, the interpreter may inline message sends at its discretion,
     * effectively replacing a call to an object's message handler with a copy
     * of that message handler's code. This reduces the overhead of sending
     * messages, and it allows further optimization because the inlined copy of
     * the code can be optimized in the context in which it is inlined, where,
     * among other things, the interpreter knows what the message is going to
     * be.
     *
     * This optimization is most effective when used in conjunction with
     * constant elision, because message sends to an object through a symbol can
     * only be inlined if the value of the symbol is first determined to be a
     * constant.
     *
     * This option is disabled by default.
     */
    bool inlining;
} BlimpOptions;

extern const BlimpOptions DEFAULT_BLIMP_OPTIONS;

extern const char *BLIMP_OPTIONS_USAGE;
    ///< A string describing the valid option strings and the settings they
    ///  control. This is essentially a usage string for Blimp_ParseOption().

/**
 * \brief Parse the option specified by a string.
 *
 * If the option string is valid, `options` is updated with the settings
 * specified by the string, and the return value is `NULL`. If the string is not
 * a valid option string, the result is a pointer to a statically allocated
 * error message string, and `options` is unchanged.
 *
 * Option strings can take one of two forms:
 *  * For boolean options, the name of the option with underscores replaced by
 *    hyphens enables the option. For example, "option-name" enables
 *    `option_name`. A "no-" prefix disable the option. So "no-option-name"
 *    disables "option_name".
 *  * For scalar option, a value must be given, separated from the name of the
 *    option by "=". For example, "option-name=42" sets `option_name` to 42.
 */
const char *Blimp_ParseOption(const char *option, BlimpOptions *options);

/**
 * \brief Update `options` to enable optional optimizations.
 */
void Blimp_OptimizationsOn(BlimpOptions *options);

/**
 * \brief Create a new `bl:mp` interpreter.
 *
 * \param options
 *      A pointer to a BlimpOptions object controlling various properties and
 *      tunable parameters of the interpreter.
 *      <br>
 *      The recommended way to set options is to copy `DEFAULT_BLIMP_OPTIONS`,
 *      modify the fields of interest, and pass a pointer to the copy to
 *      Blimp_New(). This technique only requires explicitly setting the fields
 *      you want to change, and it should be forwards compatible with new
 *      options being added.
 *      <br>
 *      Passing NULL here is the same as passing `&DEFAULT_BLIMP_OPTIONS`.
 */
Blimp *Blimp_New(const BlimpOptions *options);

/**
 * \brief Destroy a Blimp and its associated resources.
 *
 * After this call returns, you may not use `blimp` _or_ any objects associated
 * with the destroyed Blimp (such as expressions that it parsed) _except that_:
 *  * expressions created by this Blimp may be passed to Blimp_FreeExpr (in
 *    fact, they must be in order to clean up their resources, since the Blimp
 *    itself does not keep track of all the expressions it has created).
 */
void Blimp_Delete(Blimp *blimp);

/**
 * @}
 *
 * \defgroup errors Errors
 *
 * Errors are created and stored by the interpreter. This allows us to store
 * rich information (such as a message and source location) with each error. All
 * bl:mp functions which might fail return a BlimpStatus, which is an opaque
 * code. Clients can check if `status == BLIMP_OK` to see if their operation
 * succeeded, or `status != BLIMP_OK` to see if it failed. If `status !=
 * BLIMP_OK`, information about the error can be obtained by calling
 * Blimp_GetLastError.
 *
 * There are also functions (Blimp_Error*) which users can use to generate their
 * own errors. These functions take information about the error, store it in the
 * Blimp structure to be retrieved later with Blimp_GetLastError, and return a
 * non-BLIMP_OK status code.
 *
 * @{
 */
typedef struct BlimpErrorInfo *BlimpStatus;
#define BLIMP_OK ((BlimpStatus)0)

typedef enum BlimpErrorCode {
    // Parsing errors
    BLIMP_INVALID_CHARACTER,
    BLIMP_UNEXPECTED_TOKEN,
    BLIMP_UNEXPECTED_EOF,
    BLIMP_INVALID_MESSAGE_NAME,
    BLIMP_AMBIGUOUS_PARSE,

    // Runtime errors
    BLIMP_NO_SUCH_SYMBOL,
    BLIMP_NO_SUCH_METHOD,
    BLIMP_MUST_BE_BLOCK,
    BLIMP_MUST_BE_EXTENSION,
    BLIMP_MUST_BE_SYMBOL,
    BLIMP_INVALID_OBJECT_TYPE,
    BLIMP_STACK_OVERFLOW,
    BLIMP_ILLEGAL_SCOPE,
    BLIMP_OPTIMIZED_AWAY,

    // API errors
    BLIMP_VALUE_IS_IMMUTABLE,

    // Interrupts
    BLIMP_INTERRUPTED,

    // Internal consistency errors
    BLIMP_INVALID_EXPR,

    // Generic errors
    BLIMP_OUT_OF_MEMORY,
    BLIMP_IO_ERROR,
    BLIMP_NOT_SUPPORTED,
    BLIMP_ERROR,
} BlimpErrorCode;

typedef struct {
    const char *file;
    size_t row;
    size_t col;
} BlimpSourceLoc;

typedef struct {
    BlimpSourceLoc start;
    BlimpSourceLoc end;
} BlimpSourceRange;

/**
 * \defgroup stack_trace Stack Traces
 *
 * Errors which occur during the execution of `bl:mp` code have associated stack
 * traces, which can be used to obtain detailed information about where in the
 * `bl:mp` program the error occurred.
 *
 * @{
 */

typedef struct BlimpStackTrace BlimpStackTrace;

/**
 * \brief Save the current call stack for inspection.
 *
 * If successful, `*trace` points to a dynamically allocated BlimpStackTrace
 * object which is now considered owned by the caller. The caller is responsible
 * for eventually freeing the memory allocated to `*trace` by calling
 * Blimp_FreeStackTrace().
 */
BlimpStatus Blimp_SaveStackTrace(Blimp *blimp, BlimpStackTrace **trace);

/**
 * \brief Copy a saved stack trace.
 *
 * If successful, `*to` points to a BlimpStackTrace which is equivalent to but
 * independent from `from`. `*to` has its own independent lifetime.
 *
 * The memory needed to store `*to` is dynamically allocated by this function.
 * The caller is responsible for eventually freeing the memory allocated to
 * `*to` by calling Blimp_FreeStackTrace().
 */
BlimpStatus Blimp_CopyStackTrace(
    Blimp *blimp, const BlimpStackTrace *from, BlimpStackTrace **to);

/**
 * \brief Free memory allocated to a stack trace.
 */
void Blimp_FreeStackTrace(Blimp *blimp, BlimpStackTrace *trace);

/**
 * \brief Move one level higher up a stack trace.
 *
 * This affects the current frame which can be inspected using
 * BlimpStackTrace_GetRange().
 *
 * \returns
 *      `true` if the stack trace was successful moved up one frame; `false` if
 *      the stack trace is already at the top frame.
 */
bool BlimpStackTrace_Up(BlimpStackTrace *trace);

/**
 * \brief Move one level lower down a stack trace.
 *
 * This affects the current frame which can be inspected using
 * BlimpStackTrace_GetRange().
 *
 * \returns
 *      `true` if the stack trace was successful moved down one frame; `false`
 *      if the stack trace is already at the bottom frame.
 */
bool BlimpStackTrace_Down(BlimpStackTrace *trace);

/**
 * \brief
 *      Get the location in the source code corresponding to the current frame.
 */
void BlimpStackTrace_GetRange(
    const BlimpStackTrace *trace, BlimpSourceRange *range);

/**
 * \brief Pretty-print a stack trace.
 *
 * The `limit` parameter controls how much of the trace is printed, if the trace
 * is large.
 *
 * If `limit` is nonzero, at most `limit` frames are printed from the beginning
 * of the trace, and at most `limit` frames are printed from the end of the
 * trace. Missing frames will be indicated with an ellipsis, as well as by a
 * discontinuity in the stack depth printed with each frame.
 *
 * If `limit` is zero, the entire stack trace will be printed.
 */
void BlimpStackTrace_Print(
    FILE *file, const BlimpStackTrace *trace, size_t limit);

/**
 * @}
 */

/**
 * \brief Generate an error status with a given error code.
 */
BlimpStatus Blimp_Error(Blimp *blimp, BlimpErrorCode code);


/**
 * \brief Generate an error status with a given error code.
 *
 * The error will contain a snapshot of the current stack trace from `blimp`.
 */
BlimpStatus Blimp_RuntimeError(Blimp *blimp, BlimpErrorCode code);

/**
 * \brief Generate an error status with a given error code and message.
 *
 * \param[in] fmt A printf-style format string for the error message.
 * \param[in] ... Arguments for formatting the error message.
 */
BlimpStatus Blimp_ErrorMsg(
    Blimp *blimp, BlimpErrorCode code, const char *fmt, ...)
        __attribute__((format (printf, 3, 4)));


/**
 * \brief Generate an error status with a given error code and message.
 *
 * \param[in] fmt A printf-style format string for the error message.
 * \param[in] ... Arguments for formatting the error message.
 *
 * The error will contain a snapshot of the current stack trace from `blimp`.
 */
BlimpStatus Blimp_RuntimeErrorMsg(
    Blimp *blimp, BlimpErrorCode code, const char *fmt, ...)
        __attribute__((format (printf, 3, 4)));

/**
 * \brief
 *      Generate an error status with a given source location, error code and
 *      message.
 *
 * \param[in] fmt A printf-style format string for the error message.
 * \param[in] ... Arguments for formatting the error message.
 */
BlimpStatus Blimp_ErrorAt(
    Blimp *blimp,
    BlimpSourceLoc loc,
    BlimpErrorCode code,
    const char *fmt,
    ...)
        __attribute__((format (printf, 4, 5)));

/**
 * \brief
 *      Generate an error status with a given source location, error code and
 *      message.
 *
 * \param[in] fmt A printf-style format string for the error message.
 * \param[in] ... Arguments for formatting the error message.
 *
 * The error will contain a snapshot of the current stack trace from `blimp`.
 */
BlimpStatus Blimp_RuntimeErrorAt(
    Blimp *blimp,
    BlimpSourceLoc loc,
    BlimpErrorCode code,
    const char *fmt,
    ...)
        __attribute__((format (printf, 4, 5)));

/**
 * \brief
 *      Generate an error status with a given source range, error code and
 *      message.
 *
 * \param[in] fmt A printf-style format string for the error message.
 * \param[in] ... Arguments for formatting the error message.
 */
BlimpStatus Blimp_ErrorFrom(
    Blimp *blimp,
    BlimpSourceRange range,
    BlimpErrorCode code,
    const char *fmt,
    ...)
        __attribute__((format (printf, 4, 5)));

/**
 * \brief
 *      Generate an error status with a given source range, error code and
 *      message.
 *
 * \param[in] fmt A printf-style format string for the error message.
 * \param[in] ... Arguments for formatting the error message.
 *
 * The error will contain a snapshot of the current stack trace from `blimp`.
 */
BlimpStatus Blimp_RuntimeErrorFrom(
    Blimp *blimp,
    BlimpSourceRange range,
    BlimpErrorCode code,
    const char *fmt,
    ...)
        __attribute__((format (printf, 4, 5)));

/**
 * \brief Return the error status for the last error recorded in `blimp`.
 *
 * \pre One of the `Blimp_Error*` functions has been called.
 */
BlimpStatus Blimp_Reraise(Blimp *blimp);

/**
 * \brief Return the error status for the last error recorded in `blimp`.
 *
 * If the error info associated with the status does not have a stack trace,
 * then the current interpreter stack trace is attached.
 *
 * \pre One of the `Blimp_Error*` functions has been called.
 */
BlimpStatus Blimp_RuntimeReraise(Blimp *blimp);

/**
 * \brief Update the last error status with a source location.
 *
 * \pre One of the `Blimp_Error*` functions has been called.
 */
BlimpStatus Blimp_ReraiseFrom(Blimp *blimp, BlimpSourceRange range);

/**
 * \brief Print information about the last error recorded in `blimp`.
 */
void Blimp_DumpLastError(Blimp *blimp, FILE *f);

/**
 * \brief Print an error message and abort execution if `status != BLIMP_OK`.
 */
void Blimp_Check(BlimpStatus status);

/**
 * \brief Get information about the last error recorded in `blimp`.
 *
 * \param[in]  blimp   The interpreter to query.
 * \param[out] range   The source range of the error (if available) or NULL.
 * \param[out] trace
 *      The stack trace where the error occurred (if available) or NULL. If
 *      `*trace` is not NULL after this function returns, then it contains a
 *      copy of the stack trace saved with the error. The caller is responsible
 *      for eventually freeing the copied memory by calling
 *      Blimp_FreeStackTrace().
 * \param[out] message The error message (may be "" if no message is available).
 * \return The error code.
 */
BlimpErrorCode Blimp_GetLastError(
    Blimp *blimp,
    const BlimpSourceRange **range,
    BlimpStackTrace **trace,
    const char **message);

/**
 * \brief Get the code of the last error recorded in `blimp`.
 */
BlimpErrorCode Blimp_GetLastErrorCode(Blimp *blimp);

/**
 * @}
 *
 * \defgroup expressions Expressions
 * @{
 */

/**
 * Symbols are unique (per interpreter) representations of strings. With a
 * single bl:mp interpreter, two symbols represent the same string if and only
 * if they point to the same object.
 */
typedef struct BlimpSymbol BlimpSymbol;

/**
 * \brief Get the unique symbol representing the given string.
 *
 * \param[in]  blimp  The interpreter to query.
 * \param[in]  name   The null-termiated string to look up.
 * \param[out] symbol The unique Symbol representation of `name`.
 */
BlimpStatus Blimp_GetSymbol(
    Blimp *blimp, const char *name, const BlimpSymbol **symbol);

const char *BlimpSymbol_GetName(const BlimpSymbol *symbol);
size_t BlimpSymbol_Hash(const BlimpSymbol *symbol);

static inline bool BlimpSymbol_Eq(
    const BlimpSymbol *sym1, const BlimpSymbol *sym2)
{
    return sym1 == sym2;
}

typedef struct BlimpExpr BlimpExpr;

BlimpExpr *BlimpExpr_Borrow(BlimpExpr *expr);

BlimpStatus BlimpExpr_NewSymbol(
    Blimp *blimp, const BlimpSymbol *sym, BlimpExpr **expr);
BlimpStatus BlimpExpr_NewBlock(
    Blimp *blimp,
    const BlimpSymbol *msg_name,
    BlimpExpr *code,
    BlimpExpr **expr);
BlimpStatus BlimpExpr_NewSend(
    Blimp *blimp, BlimpExpr *receiver, BlimpExpr *message, BlimpExpr **expr);

void BlimpExpr_SetSourceRange(BlimpExpr *expr, const BlimpSourceRange *range);

/**
 * \brief Free memory associated with an expression.
 *
 * The expression must have been created by a Blimp function, such as
 * Blimp_Parse. The expression will be destroyed and freed, and the contents of
 * `expr` may not be used after Blimp_FreeExpr returns.
 */
void Blimp_FreeExpr(BlimpExpr *expr);

/**
 * \brief Write a human-readable representation of `expr` to `file`.
 *
 * This function produces `bl:mp` code representing `expr` and writes it to
 * `file`, which must be open for writing. The resulting code would yield `expr`
 * again if parsed using Blimp_Parse().
 */
void Blimp_PrintExpr(Blimp *blimp, FILE *file, const BlimpExpr *expr);

/**
 * \brief Write a human-readable representation of `expr` to `file`.
 *
 * This function expresses an expression textually using the language of the
 * semi-formal semantic model for bl:mp (/docs/semantics.rkt).
 */
void Blimp_DumpExpr(Blimp *blimp, FILE *file, const BlimpExpr *expr);

/**
 * @}
 *
 * \defgroup parsing Parsing
 * @{
 */

/**
 * The bl:mp parser gets its input from a BlimpStream, which is a polymorphic
 * object representing a text input (file, string, socket, etc.).
 *
 * This library provides two kinds of streams out of the box: Blimp_FileStream
 * returns streams which read from files, and Blimp_StringStream returns streams
 * which provide input to the parser from an in-memory string. Users can also
 * write their own streams to parse a custom input source. To do so, create a
 * struct whose first field is of type BlimpStream, and initialize the three
 * function pointers in that field to suitable functions.
 */
typedef struct BlimpStream {
    /**
     * \brief Advance the stream one character.
     *
     * \param[in]  self The stream to advance.
     * \param[out] c    The character produced, or EOF.
     */
    BlimpStatus (*Next)(struct BlimpStream *self, int *c);

    /**
     * \brief Retrieve the source location of the next character in the stream.
     *
     * The location is used for error reporting only. bl:mp does not ever rely
     * on it being accurate or meaningful. Streams which are not reading from a
     * file can use NULL as the filename.
     */
    BlimpSourceLoc (*Location)(struct BlimpStream *self);

    /**
     * \brief Close a stream and clean up its resources.
     */
    void (*Close)(struct BlimpStream *self);

#ifndef DOXYGEN
    // Reserved for internal use. These fields allow us to implement peeking,
    // which returns the next character in the stream without consuming it.
    int peek;
    bool peek_valid;
    BlimpSourceLoc peek_loc;
#endif
} BlimpStream;

/**
 * \brief Create a stream which reads input from the file `path`.
 */
BlimpStatus Blimp_FileStream(
    Blimp *blimp, const char *path, BlimpStream **stream);

/**
 * \brief Create a stream which reads input from an open file.
 *
 * The stream will close the file when parsing finishes.
 *
 * \param[in]  blimp The interpreter state.
 * \param[in]  name  The name of the file. This is used for error messages only.
 * \param[in]  file  The file to read from.
 * \param[out] stream The new stream.
 *
 * \pre `file` must be opened for reading.
 */
BlimpStatus Blimp_OpenFileStream(
    Blimp *blimp, const char *name, FILE *file, BlimpStream **stream);

/**
 * \brief Create a stream which reads input from `str`.
 */
BlimpStatus Blimp_StringStream(
    Blimp *blimp, const char *str, BlimpStream **stream);

/**
 * \brief Parse the contents of `input` and construct a `BlimpExpr`.
 *
 * \note
 *      Blimp_Parse will close `input` by calling `input->Close(input)` when
 *      parsing is done.
 */
BlimpStatus Blimp_Parse(Blimp *blimp, BlimpStream *input, BlimpExpr **output);

/**
 * \brief Parse the contents of the file `path` and construct a `BlimpExpr`.
 */
BlimpStatus Blimp_ParseFile(Blimp *blimp, const char *path, BlimpExpr **output);

/**
 * \brief Parse the contents of `str` and construct a `BlimpExpr`.
 */
BlimpStatus Blimp_ParseString(Blimp *blimp, const char *str, BlimpExpr **output);

void Blimp_DumpGrammarVitals(FILE *file, Blimp *blimp);

/**
 * @}
 *
 * \defgroup compiling Compiling
 *
 * Blimp expressions are compiled internally to a low-level, assembly-like
 * bytecode language which is executed by the machine.
 *
 * This interface allows the programmer to manipulate and inspect sequences of
 * bytecode instructions directly.
 *
 * @{
 */

/**
 * \brief A sequence of bytecode instructions.
 *
 * BlimpBytecode objects can be shared (internally, for example, a single
 * bytecode sequence may be shared by many instances of a block with the same
 * expression.) Therefore, to easy memory management, they are reference
 * counted. Each function which takes or returns a BlimpBytecode object will
 * document its effect on the reference count. Once the reference count of a
 * BlimpBytecode object reaches 0, the object may no longer be used.
 */
typedef struct BlimpBytecode BlimpBytecode;

/**
 * \brief Compile an expression to bytecode.
 *
 * If compilation succeeds, then `*code` is a new bytecode object with a
 * reference count of 1. The reference is considered to be owned by the caller,
 * so the caller is now responsible for eventually calling BlimpBytecode_Free.
 */
BlimpStatus BlimpExpr_Compile(
    Blimp *blimp, BlimpExpr *expr, BlimpBytecode **code);

/**
 * \brief Release a reference to a BlimpBytecode object.
 */
void BlimpBytecode_Free(BlimpBytecode *code);

/**
 * \brief Increment the reference count of a BlimpBytecode object.
 */
void BlimpBytecode_Borrow(BlimpBytecode *code);

/**
 * \brief Get the bytecode currently being executed in the global scope.
 *
 * If no code is currently being executed, the result is `NULL`. Otherwise, the
 * result is a BlimpBytecode object. The caller does not own a reference to the
 * returned bytecode object, so the object may be destroyed or invalidated after
 * any subsequent `bl:mp` API call (except BlimpBytecode_Print()). To persist
 * their reference to the bytecode, the caller must call BlimpBytecode_Borrow().
 */
BlimpBytecode *Blimp_GlobalBytecode(Blimp *blimp);

/**
 * \brief Print a sequence of bytecode.
 *
 * \param file
 *      A file stream, which must be open for writing, to which to write the
 *      code.
 * \param code
 *      The bytecode procedure to write.
 * \param recursive
 *      If `true`, then other bytecode procedures referenced by `code` will also
 *      be printed, after `code`. Otherwise, only `code` will be printed, and
 *      references to other bytecode procedures will be represented in `code` as
 *      opaque numberic identifiers.
 */
void BlimpBytecode_Print(
    FILE *file, const BlimpBytecode *code, bool recursive);

/**
 * @}
 *
 * \defgroup objects Objects
 *
 * A BlimpObject is the runtime representation of a value. It is the result of
 * succesfully evaluating a BlimpExpr.
 *
 * There are two kinds of objects in core bl:mp, and one additional kind
 * provided by the bl:mp:C implementation to allow programmers to extend the
 * basic functionality of the interpreter. The two kinds of core objects are
 * symbol objects (e.g. the result of evaluating `foo`) and block objects (e.g.
 * the result of evaluating `{a|b}`). The bl:mp:C interpreter also provides
 * extension objects, which are like block objects, except instead of containing
 * bl:mp code that executes when they receive a message, extension objects
 * contain C code.
 *
 * All objects have in common the following properites:
 *  * scope:
 *      a mutable map from symbols to objects
 *  * parent:
 *      a parent scope. If a symbol is not in scope in the object's own scope,
 *      it will be looked up in the parent scope. Only one object, the global
 *      scope, does not have a parent. It is an error to look up a symbol that
 *      does not exist in the global scope.
 *  * refcount:
 *      the number of times BlimpObject_Release() must be called on that object
 *      before it is destroyed. The reference count cannot be inspected or
 *      manipulated directly, but the BlimpObject_Borrow() and
 *      BlimpObject_Release() APIs can be used to increment and decrement the
 *      reference count, respectively.
 *
 * A few useful bits of notation:
 *  * Many BlimpObject_* functions require a "managed" object. This is an object
 *    with a nonzero reference count, which was allocated from the bl:mp
 *    interpreter object pool corresponding to some Blimp object `b` (for
 *    example, by BlimpObject_NewBlock or BlimpObject_NewSymbol, or as the
 *    result of Blimp_Eval). We say such an object is "managed by `b`".
 *  * Many functions are documented to return a "fresh" object: this is simply a
 *    managed object with a reference count of 1, such as a newly created
 *    object.
 *  * Some functions are documented to return a "new reference". This means that
 *    the caller now owns a reference to the object which was returned, and it
 *    is their responsibility to eventually call BlimpObject_Release() on that
 *    reference. Until they do so, they can trust that the reference will remain
 *    valid.
 *  * Other functions return a "transient reference". This contrasts with a "new
 *    reference" in that the callee, not the caller, is responsible for managing
 *    the reference. If the caller wants the object to persist independently of
 *    the callee, they must call BlimpObject_Borrow() explicitly. Typically,
 *    functions that return a transient reference will guarantee that it lives
 *    "at least as long as" some other reference, which the caller likely owns.
 *    If the caller is satisfied with this lifetime, they can use the transient
 *    reference without ever calling BlimpObject_Borrow() and
 *    BlimpObject_Release().
 *
 * @{
 */

/**
 * \struct BlimpObject
 * \copydoc objects
 */
typedef struct BlimpObject BlimpObject;

/**
 * \brief
 *      The type of the C function which handles messages sent to extension
 *      objects.
 */
typedef BlimpStatus(*BlimpMethod)(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result);

/**
 * \brief Cleanup function for extension objects.
 *
 * A finalizer is a function that runs when a bl:mp extension object is garbage
 * collected. It can be used to clean up resources owned by the object's
 * associated state pointer.
 *
 * Note that finalizers may run when the bl:mp heap is in an inconsistent state
 * (for example, in the middle of a garbage collection sweep), so these
 * functions should not use the bl:mp API, or perform any operations besides
 * resource cleanup.
 */
typedef void(*BlimpFinalizer)(void *);

/**
 * \defgroup objects_creating Creating Objects
 *
 * These constructor functions allocate and initialize the various kinds of
 * objects. All constructors take an interpreter and a parent object, as well as
 * type-specific object properties (e.g. code for blocks; symbol for symbols).
 * The scope of the new object is initially empty.
 *
 * @{
 */

/**
 * \brief Create a new block object.
 * \returns a fresh object
 */
BlimpStatus BlimpObject_NewBlock(
    Blimp *blimp,
    BlimpObject *parent,
    const BlimpSymbol *msg_name,
    BlimpExpr *code,
    BlimpObject **obj);


/**
 * \brief Create a new extension object.
 * \returns a fresh object
 */
BlimpStatus BlimpObject_NewExtension(
    Blimp *blimp,
    BlimpObject *parent,
    void *state,
    BlimpMethod code,
    BlimpFinalizer finalize,
    BlimpObject **obj);


/**
 * \brief Create a new symbol object.
 * \returns a fresh object
 */
BlimpStatus BlimpObject_NewSymbol(
    Blimp *blimp,
    const BlimpSymbol *sym,
    BlimpObject **obj);

/**
 * \brief Increment the reference count of an object.
 *
 * This function registers a new managed owner of `obj`. Conceptually, it
 * increments the reference count of `obj`. If, before this call, the `n`th call
 * to BlimpObject_Release would have destroyed `obj`, then after this call,
 * `obj` will not be destroyed until the `n+1`th call to BlimpObject_Release.
 *
 * \returns a new reference to `obj`
 */
BlimpObject *BlimpObject_Borrow(BlimpObject *obj);

/**
 * \brief Decrement the reference count of an object
 *
 * This function relinqushes shared ownership of an object. If the caller is the
 * last owner of the object (that is, if the reference count before the call was
 * 1), thens the object will be destroyed and returned to the interpreter for
 * possible future reuse. Otherwise, the reference count is decremented.
 *
 * Since the caller cannot necessarily tell if they are the last owner of the
 * object, they must never use `obj` again after this function is called.
 */
void BlimpObject_Release(BlimpObject *obj);

/**
 * @}
 *
 * \defgroup objects_global The Global Object
 *
 * @{
 */

/**
 * \brief Get a reference to the global object.
 *
 * \returns a transient reference (lives as long as the interpreter)
 */
BlimpObject *Blimp_GlobalObject(Blimp *blimp);

/**
 * @}
 *
 * \defgroup objects_inspecting Inspecting Objects
 *
 * @{
 */

/**
 * \brief Print a legible representation of `obj` to a file.
 */
void BlimpObject_Print(FILE *file, const BlimpObject *obj);

/**
 * \brief Retrieve the type-specific properties of a block object.
 *
 * \param[in]   obj     The object to inspect (must be a block object).
 * \param[out]  code    If not `NULL`, will be initialized to point to the block
 *                      object's code.
 *
 * \par Errors
 *  * `BLIMP_MUST_BE_BLOCK`:
 *      `obj` was not a block object. The contents of `*code` are
 *      undefined.
 */
BlimpStatus BlimpObject_ParseBlock(
    const BlimpObject *obj, BlimpBytecode **code);

/**
 * \brief Retrieve the type-specific properties of an extension object.
 *
 * \param[in]   obj     The object to inspect (must be an extension object).
 * \param[out]  method  The method used by this object to handle messages.
 * \param[out]  state   The arbitrary, user-maintained state associated with
 *                      this object.
 *
 * \par Errors
 *  * `BLIMP_MUST_BE_EXTENSION`:
 *      `obj` was not an extension object. The contents of `*method` and
 *      `*state` are undefined.
 */
BlimpStatus BlimpObject_ParseExtension(
    const BlimpObject *obj, BlimpMethod *method, void **state);

/**
 * \brief Retrieve the type-specific properties of a symbol object.
 *
 * \param[in]   obj     The object to inspect (must be a symbol object).
 * \param[out]  sym     If not `NULL`, will be initialized to point to the
 *                      symbol object's value.
 *
 * \par Errors
 *  * `BLIMP_MUST_BE_SYMBOL`:
 *      `obj` was not a symbol object. The contents of `*sym` are undefined.
 */
BlimpStatus BlimpObject_ParseSymbol(
    const BlimpObject *obj, const BlimpSymbol **sym);

typedef struct {
    size_t refcount;
    BlimpObject *clump;
    size_t clump_refcount;
} BlimpObjectInfo;

/**
 * \brief Get information about the internal state of an object.
 */
void BlimpObject_Inspect(BlimpObject *obj, BlimpObjectInfo *info);

/**
 * \brief Iterate over children of an object.
 *
 * For each child object `child` which is referenced by `obj`, `func` is passed
 * the following:
 *
 *  \param blimp        The bl:mp interpreter.
 *  \param obj          The parent object.
 *  \param child_name   The name by which the parent object owns the child.
 *  \param child        The child object.
 *  \param arg          Arbitrary data passed to BlimpObject_ForEachChild().
 */
void BlimpObject_ForEachChild(
    BlimpObject *obj,
    void(*func)(
        Blimp *blimp,
        BlimpObject *obj,
        const BlimpSymbol *child_name,
        BlimpObject *child,
        void *arg),
    void *arg);

/**
 * @}
 *
 * \defgroup objects_primitives Primitive Operations
 *
 * @{
 */

/**
 * \brief Get the value of a symbol in an object's scope.
 *
 *  * The symbol `sym` is looked up in the scope of `obj`. If it is found, it is
 *    returned.
 *  * Otherwise, if `obj` has a parent object, the symbol is looked up in the
 *    scope of the parent (and the parent's parent, and so on, as if by calling
 *    BlimpObject_Get recursively on the scope of the parent).
 *  * If the symbol is not found in the object's scope or the scope of any of
 *    its parents, an error is returned.
 *
 * \returns
 *      a transient reference (lives at least as long as `obj`, or until the
 *      next call to BlimpObject_Set(), whichever is shorter)
 *
 * \par Errors
 *  * `BLIMP_NO_SUCH_SYMBOL`:
 *      the symbol was not found in the scope of `obj` or any of its parents.
 *  * `BLIMP_INVALID_OBJECT_TYPE`:
 *      `obj` is not a scoped object (for example, it is a symbol).
 */
BlimpStatus BlimpObject_Get(
    BlimpObject *obj, const BlimpSymbol *sym, BlimpObject **ret);

/**
 * \brief Set the value of a symbol in an object's scope.
 *
 *  * If the symbol exists in the scope of `obj` or any of its parents (that is,
 *    if `BlimpObject_Get(obj, sym, &val)` would succeed) then the value of the
 *    symbol is replaced by `val` in the _innermost_ scope containing the
 *    symbol. If the symbol exists in more than one scope among `obj` and all of
 *    its parents, it is set in `obj` if possible, or in the immediate parent of
 *    `obj` if possible, and so on.
 *  * Otherwise, if the symbol does not exist in the scope of `obj` or any of
 *    its parents, it is added to the scope of `obj` and associated with the
 *    given value.
 *
 * The implementation of the function automatically increments the reference
 * count of `val`, as if by calling BlimpObject_Borrow(). This means that the
 * caller keeps their managed reference to `val`, so they must still eventually
 * call BlimpObject_Release(). It also decrements the reference count of the old
 * value, as if by calling BlimpObject_Release(), to release the reference that
 * was borrowed by BlimpObject_Set() when that old value was set.
 *
 * \par Errors
 *  * `BLIMP_OUT_OF_MEMORY`
 *  * `BLIMP_INVALID_OBJECT_TYPE`:
 *      `obj` is not a scoped object (for example, it is a symbol).
 *  * `BLIMP_VALUE_IS_IMMUTABLE`:
 *      The value of `sym` in the scope of `obj` is immutable, because the
 *      runtime has detected that all references to that value have been
 *      destroyed using constant elision (option `constant_elision`).
 */
BlimpStatus BlimpObject_Set(
    BlimpObject *obj, const BlimpSymbol *sym, BlimpObject *val);

BlimpStatus BlimpObject_CaptureMessage(BlimpObject *obj, BlimpObject *message);

/**
 * \brief Get a message which is captured by an object.
 *
 * `index` is the DeBruijn index of the message. This refers to the number of
 * nested scopes between `obj` and the scope where the desired message was
 * defined.
 *
 * `obj` must be a lexical closure, which means it must be a block or extension
 * object.
 *
 * \return a transient reference (lives at least as long as `obj`)
 *
 * \par Errors
 *  * `BLIMP_INVALID_MESSAGE_NAME`:
 *      `index` exceeds the nesting level of the scope of `obj`.
 *  * `BLIMP_INVALID_OBJECT_TYPE`:
 *      `obj` is not a scoped object (for example, it is a symbol).
 *  * `BLIMP_OPTIMIZED_AWAY`:
 *      the message was not captured because it was not used in the source code.
 */
BlimpStatus BlimpObject_GetCapturedMessage(
    BlimpObject *obj, size_t index, BlimpObject **message);

/**
 * \brief Get a message which is captured by an object.
 *
 * `obj` must be a lexical closure, which means it must be a block or extension
 * object.
 *
 * \return a transient reference (lives at least as long as `obj`)
 *
 * \par Errors
 *  * `BLIMP_INVALID_MESSAGE_NAME`:
 *      `name` does not refer to a caputred message in the scope of `obj`.
 *  * `BLIMP_INVALID_OBJECT_TYPE`:
 *      `obj` is not a scoped object (for example, it is a symbol).
 *  * `BLIMP_OPTIMIZED_AWAY`:
 *      the message was not captured because it was not used in the source code.
 */
BlimpStatus BlimpObject_GetCapturedMessageByName(
    BlimpObject *obj, const BlimpSymbol *name, BlimpObject **message);

/**
 * @}
 *
 * \defgroup Garbage Collection
 *
 * All objects managed by a bl:mp interpreter are garbage collected, meaning
 * they are automatically cleaned up when they are no longer reachable from
 * managed references (that is, they have reference count 0) or from other
 * reachable objects.
 *
 * For the most part, this happens transparently, without input from the user.
 * However, there is an API that can lend some visibility into the garbage
 * collector.
 *
 * @{
 */

typedef struct {
    size_t created;
        ///< The total number of objects ever created.

    size_t allocated;
        ///< The total number of objects allocated right now.
    size_t reachable;
        ///< The number of currently allocated objects which are reachable from
        ///  a live object (that is, one with a nonzero reference count).
    size_t max_allocated;
        ///< The largest number of objects which were ever allocated at one
        ///  time.

    size_t clumps;
        ///< The number of allocated entanglement clumps.
    size_t entangled;
        ///< The number of allocated objects belonging to a clump.
    size_t max_clump;
        ///< The number of objects in the largest entanglement clump.
    size_t min_clump;
        ///< The number of objects in the smallest entanglement clump.

    size_t collections;
        ///< The number of times the tracing garbage collector has run.
} BlimpGCStatistics;

BlimpGCStatistics Blimp_GetGCStatistics(Blimp *blimp);

/**
 * \brief Write a human-readable description of the heap to `f`.
 */
void Blimp_DumpHeap(FILE *f, Blimp *blimp);

/**
 * \brief Write a human-readable description of the heap to `f`.
 *
 * Unlike Blimp_DumpHeap(), this function will only print information about
 * objects which are unreachable. This can be useful for debugging memory leaks
 * and other garbage collection-related problems.
 */
void Blimp_DumpUnreachable(FILE *f, Blimp *blimp);

/**
 * \brief Force a garbage collection sweep.
 */
void Blimp_CollectGarbage(Blimp *blimp);

/**
 * \brief Iterate over garbage collected objects.
 *
 * For each currently allocated object in the garbage collected heap, `func` is
 * called and passed `blimp`, the current object, and `arg`.
 *
 * Note that this iteration only includes garbage collected objects. As an
 * optimization, some objects (most notably symbol objects) are exempted from
 * garbage collection, and cannot be reached this way.
 */
void Blimp_ForEachObject(
    Blimp *blimp,
    void(*func)(Blimp *blimp, BlimpObject *obj, void *arg),
    void *arg);

/**
 * @}
 * @}
 *
 * \defgroup interpreting Interpreting bl:mp Programs
 *
 * @{
 */

/**
 * \brief Interpret a bl:mp expression.
 *
 * \param[in] blimp
 *      The interpreter.
 * \param[in] expr
 *      The expression to evaluate.
 * \param[in] scope
 *      The object in whose scope to do the evaluation.
 * \param[out] result
 *      A new reference.
 */
BlimpStatus Blimp_Eval(
    Blimp *blimp,
    BlimpExpr *expr,
    BlimpObject *scope,
    BlimpObject **result);

/**
 * \brief Evaluate a bl:mp expression and require that the result is a symbol.
 *
 * If `expr` succesfully evaluates to a symbol, then this function succeeds and
 * sets `*sym` to that symbol. If `expr` evaluates but not to a symbol, this
 * function raises the error `BLIMP_MUST_BE_SYMBOL`, and the contents of `*sym`
 * are unspecified.
 *
 * No matter what, this function does not retain a reference to the evaluated
 * Object: it evaluates the object, extracts the symbol, and then releases the
 * object.
 *
 * You can think of this as a wrapper around Blimp_Eval(),
 * BlimpObject_ParseSymbol(), and BlimpObject_Release(). However, it may be
 * implemented more efficiently to avoid the creation of temporary objects
 */
BlimpStatus Blimp_EvalSymbol(
    Blimp *blimp,
    BlimpExpr *expr,
    BlimpObject *scope,
    const BlimpSymbol **sym);

/**
 * \brief Send a message to an object.
 *
 * \param[in] blimp
 *      The interpreter.
 * \param[in] scope
 *      The object in whose scope to evaluate the message send. For most
 *      objects, this is ignored, as the body of the receiving object will be
 *      evaluated in its own scope. However, for messages sent to symbols, this
 *      is the scope in which the symbol will be looked up and possibly
 *      modified.
 * \param[in] receiver
 *      The object to receive the message.
 * \param[in] message
 *      The message to send.
 * \param[out] result
 *      A new reference.
 */
BlimpStatus Blimp_Send(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result);

/**
 * \brief Send a message to an object and require that the result is a symbol.
 */
BlimpStatus Blimp_SendAndParseSymbol(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    const BlimpSymbol **sym);

/**
 * @}
 *
 * \defgroup signals Signals
 *
 * Much like POSIX signals can be used to asynchronously inerrupt a thread,
 * `bl:mp` signals can be used to asynchronously interrupt a running `bl:mp`
 * interpreter. However, signals generated through the `bl:mp` API are
 * guaranteed not to be delivered until the interpreter is in a consistent state
 * where it can be safely interrupted. Therefore, `bl:mp` signal handlers are
 * not subject to the normal constraints on signal handlers regarding atomicity
 * of access to shared data, and they may make full use of the `bl:mp` API.
 *
 *@{
 */

/**
 * \brief The largest unsigned integer which represents a valid `bl:mp` signal.
 *
 * `BLIMP_MAX_SIGNAL` is guaranteed to be at least 6 (and on most architectures,
 * it is significantly larger, on the order of 30).
 */
#define BLIMP_MAX_SIGNAL (sizeof(sig_atomic_t)*CHAR_BIT - 2)
    // Internally, we use a bitmap of type `sig_atomic_t` to represent sets of
    // signals. The C standard guarantees that `sig_atomic_t` is at least one
    // byte, but one of the guaranteed 8 bits is reserved as the status bit,
    // which indicates whether signals are enabled. Thus, we are left with 7
    // bits that can represent signals, so the valid signal identifiers are 0-6.
    //
    // Note that in practice this will probably be somewhat larger than 6
    // (likely 30 or 62) as almost all modern architectures support atomic
    // access to 4-byte or 8-byte words.

typedef BlimpStatus(*BlimpSignalCallback)(
    Blimp *blimp, size_t signum, void *arg);

typedef enum {
    BLIMP_SIGNAL_OK,
    BLIMP_SIGNAL_INVALID,
    BLIMP_SIGNAL_DISABLED,
} BlimpSignalError;

/**
 * \brief Register a signal handler.
 *
 * After Blimp_HandleSignal() returns `BLIMP_SIGNAL_OK`, the function `callback`
 * will be registered as a handler for the signal indicated by `signum` (which
 * must not exceed `BLIMP_MAX_SIGNAL`). If a subsequent call to
 * `Blimp_RaiseSignal(blimp, signum)` returns `BLIMP_SIGNAL_OK`, then the
 * function `callback` will be called at the next time the interpreter reaches a
 * safe interruption point.
 *
 * The callback function receives the interpreter, the signal identifier, and
 * the arbitrary additional parameter `arg` as inputs.
 *
 * If the callback function returns `BLIMP_OK`, then the interpreter will
 * continue execution normally (possibly invoking handlers for other pending
 * signals). Otherwise, the interpreter will exit, returning the error code from
 * the callback function to the caller who invoked the interpreter.
 *
 * \par Errors
 *  * `BLIMP_SIGNAL_INVALID`:
 *      `signum` is greater than `BLIMP_MAX_SIGNAL`.
 */
BlimpSignalError Blimp_HandleSignal(
    Blimp *blimp, size_t signum, BlimpSignalCallback callback, void *arg);

/**
 * \brief Generate a signal.
 *
 * If Blimp_RaiseSignal() returns `BLIMP_OK`, then the interpreter will stop and
 * invoke the handler for the signal identified by `signum` at the next point
 * where it is safe to do so. If no handler is registered for `signum`, the
 * default action is to exit the interpreter with the error `BLIMP_INTERRUPTED`.
 *
 * This function is both thread-safe and signal-safe, meaning it can be used to
 * asynchronously interrupt a running interpreter from another thread or from a
 * POSIX signal handler.
 *
 * \par Errors
 *  * `BLIMP_SIGNAL_INVALID`:
 *      `signum` is greater than `BLIMP_MAX_SIGNAL`.
 *  * `BLIMP_SIGNAL_DISABLED`
 *      No signal can be raised because the interpreter is not currently
 *      running.
 */
BlimpSignalError Blimp_RaiseSignal(Blimp *blimp, size_t signum);

/**
 * @}
 */

#endif // BLIMP_H
