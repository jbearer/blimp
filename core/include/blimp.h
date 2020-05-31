#ifndef BLIMP_H
#define BLIMP_H

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
     *      {outer_scope|
     *          local{:=|value}
     *      }
     *
     * The `outer_scope` object clearly has a reference to `value` through
     * `local` in its scope. But `value` has a reference to the `:=` object,
     * which is its parent. And that object has a reference to the `outer_scope`
     * object, creating a cycle. `gc_refcount` on its own would not be able to
     * free any of the objects involved in this cycle; we would need
     * `gc_tracing` to do so. But with `gc_cycle_detection` enabled, we will
     * detect that these objects form a cycle, and we will free the whole group
     * of objects as soon as all references to objects in the cycle originate
     * from within the cycle; that is, as soon as there are no references from
     * outside the cycle to any of the objects in it.
     *
     * This option is enabled by default.
     */
    bool gc_cycle_detection;
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
#define BLIMP_OK ((BlimpStatus)NULL)

typedef enum BlimpErrorCode {
    // Parsing errors
    BLIMP_INVALID_CHARACTER,
    BLIMP_UNEXPECTED_TOKEN,

    // Runtime errors
    BLIMP_NO_SUCH_SYMBOL,
    BLIMP_NO_SUCH_METHOD,
    BLIMP_MUST_BE_BLOCK,
    BLIMP_MUST_BE_SYMBOL,

    // Internal consistency errors
    BLIMP_INVALID_EXPR,

    // Generic errors
    BLIMP_OUT_OF_MEMORY,
    BLIMP_IO_ERROR,
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
 * \brief Generate an error status with a given error code.
 */
BlimpStatus Blimp_Error(Blimp *blimp, BlimpErrorCode code);

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
 * \brief Generate an error status with a given source location, error code and message.
 *
 * \param[in] fmt A printf-style format string for the error message.
 * \param[in] ... Arguments for formatting the error message.
 */
BlimpStatus Blimp_ErrorAt(
    Blimp *blimp, BlimpSourceLoc loc, BlimpErrorCode code, const char *fmt, ...)
        __attribute__((format (printf, 4, 5)));

/**
 * \brief Generate an error status with a given source range, error code and message.
 *
 * \param[in] fmt A printf-style format string for the error message.
 * \param[in] ... Arguments for formatting the error message.
 */
BlimpStatus Blimp_ErrorFrom(
    Blimp *blimp, BlimpSourceRange range, BlimpErrorCode code, const char *fmt, ...)
        __attribute__((format (printf, 4, 5)));

/**
 * \brief Return the error status for the last error recorded in `blimp`.
 *
 * \pre One of the `Blimp_Error*` functions has been called.
 */
BlimpStatus Blimp_Reraise(Blimp *blimp);

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
 * \param[out] message The error message (may be "" if no message is available).
 * \return The error code.
 */
BlimpErrorCode Blimp_GetLastError(
    Blimp *blimp, const BlimpSourceRange **range, const char **message);

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

typedef struct BlimpExpr BlimpExpr;

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
void Blimp_PrintExpr(FILE *file, const BlimpExpr *expr);

/**
 * \brief Write a human-readable representation of `expr` to `file`.
 *
 * This function expresses an expression textually using the language of the
 * semi-formal semantic model for bl:mp (/docs/semantics.rkt).
 */
void Blimp_DumpExpr(FILE *file, const BlimpExpr *expr);

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

/**
 * @}
 *
 * \defgroup objects Objects
 *
 * A BlimpObject is the runtime representation of a value. It is the result of
 * succesfully evaluating a BlimpExpr.
 *
 * There are two kinds of objects in bl:mp: symbols objects (e.g. the result of
 * evaluating `foo`) and block objects (e.g. the result of evaluating `{a|b}`).
 * All objects have in common the following properites:
 *  * scope:
 *      a mutable map from symbols to objects
 *  * parent:
 *      a parent scope. If a symbol is not in scope in the object's own scope,
 *      it will be looked up in the parent scope. Only one object, the global
 *      scope, does not have a parent. It is an error to look up a symbol that
 *      does not exist in the global scope.
 * In addition, the two kinds of objects have some properties specific to that
 * kind of object:
 *
 * _Blocks_
 *  * tag:
 *      the symbol which is used for vtable lookups when the block is used as
 *  * code:
 *      the expression which is evaluated when the block is sent a `.eval`
 *      message
 *
 * _Symbols_
 *  * symbol: the value of the symbol
 *
 * In addition to the general and type-specific properties of objects, all
 * objects have a "reference count", which is the number of times
 * BlimpObject_Release() must be called on that object before it is destroyed.
 * The reference count cannot be inspected or manipulated directly, but the
 * BlimpObject_Borrow() and BlimpObject_Release() APIs can be used to increment
 * and decrement the reference count, respectively.
 *
 * A few useful bits of notation:
 *  * Many BlimpObject_* functions require a "managed" object. This is an object
 *    with a nonzero reference count, which was allocated from the bl:mp
 *    interpreter object pool corresponding to some Blimp object `b` (for
 *    example, by BlimpObject_NewBlock or BlimpObject_NewSymbol, or as the
 *    result of Blimp_Eval). We say such an object is "managed by `b`".
 *  * Many functions are documented to return a "fresh" object: this is simply
 *    an managed object with a reference count of 1, such as a newly created
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
 *    reference without ever callin BlimpObject_Borrow() and
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
 * \defgroup objects_creating Creating Objects
 *
 * These constructor functions allocate and initialize each of the two kinds of
 * objects. Both constructors take an interpreter and a parent object, as well
 * as the type-specific object properties (tag and code for blocks; symbol for
 * symbols). The scope of the new object is initially empty.
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
    const BlimpSymbol *tag,
    BlimpExpr *code,
    BlimpObject **obj);

/**
 * \brief Create a new symbol object.
 * \returns a fresh object
 */
BlimpStatus BlimpObject_NewSymbol(
    Blimp *blimp,
    BlimpObject *parent,
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
 * \brief Get the parent object of an object.
 *
 * Returns the parent of `obj`, or `NULL` if `obj` is the global object.
 *
 * \returns a transient reference (lives at least as long as `obj`)
 */
BlimpObject *BlimpObject_Parent(const BlimpObject *obj);

/**
 * \brief Get the tag of an object.
 *
 * If `obj` is a block `{tag|code}`, then this returns `tag`. If `obj` is a
 * symbol, the result is `symbol`. If `obj` is the global object, the result is
 * `NULL`.
 */
const BlimpSymbol *BlimpObject_Tag(const BlimpObject *obj);

/**
 * \brief Print a legible representation of `obj` to a file.
 */
void BlimpObject_Print(FILE *file, const BlimpObject *obj);

/**
 * \brief Retrieve the type-specific properties of a block object.
 *
 * \param[in]   obj     The object to inspect (must be a block object).
 * \param[out]  tag     If not `NULL`, will be initialized to point to the block
 *                      object's tag.
 * \param[out]  code    If not `NULL`, will be initialized to point to the block
 *                      object's code.
 *
 * \par Errors
 *  * `BLIMP_MUST_BE_BLOCK`:
 *      `obj` was not a block object. The contents of `*tag` and `*code` are
 *      undefined.
 */
BlimpStatus BlimpObject_ParseBlock(
    const BlimpObject *obj, const BlimpSymbol **tag, const BlimpExpr **code);

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

/**
 * @}
 *
 * \defgroup objects_primitives Primitive Operations
 *
 * These functions implement the three primitive bl:mp operations: `symbol.get`,
 * `symbol:=`, and `_.eval`.
 *
 * @{
 */

/**
 * \brief Get the value of a symbol in an object's scope.
 *
 * This function implements the primitive `symbol.get` operation. Specifically,
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
 */
BlimpStatus BlimpObject_Get(
    const BlimpObject *obj, const BlimpSymbol *sym, BlimpObject **ret);

/**
 * \brief Set the value of a symbol in an object's scope.
 *
 * This function implements the primitive `symbol :=` operation. Specifically,
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
 */
BlimpStatus BlimpObject_Set(
    BlimpObject *obj, const BlimpSymbol *sym, BlimpObject *val);

/**
 * \brief Evaluate the code in a block object.
 *
 * This function implements the primitive `_.eval` operation. `obj` must be a
 * block object. It's code expression is evaluated as if by calling `Blimp_Eval`
 * in the scope of `obj`, and the resulting new object is stored in `*ret`.
 *
 * \returns a new reference
 */
BlimpStatus BlimpObject_Eval(BlimpObject *obj, BlimpObject **ret);

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
} BlimpGCStatistics;

BlimpGCStatistics Blimp_GetGCStatistics(Blimp *blimp);

/**
 * \brief Force a garbage collection sweep.
 */
void Blimp_CollectGarbage(Blimp *blimp);

/**
 * @}
 * @}
 *
 * \defgroup vtable The V-Table
 *
 * The `bl:mp` V-Table maps (receiver tag, message tag) pairs to code which runs
 * when a message with that tag is sent to an object with that tag. For example,
 * the message send `{receiver|.} {message|.}` would look up the code associated
 * with `(receiver, message)` in the V-Table. Or, the message `foo{.get|.}`
 * would look up the code associated with `(symbol, .get)`.
 *
 * Truthfully, the lookup is actually slightly more complicated than this,
 * because `bl:mp` allows fallthrough receivers and fallthrough messages,
 * denoted by the wildcard symbol `_`. If a (receiver, message) lookup fails,
 * then we will try to look up (receiver, _) instead. This is a fallthrough
 * method; it allows the receiving class to handle the incoming message
 * dynamically if it does not have an explicit handler for the tag of the
 * message. If that lookup fails, then we will lookup (_, message). This is a
 * fallthrough receiver: it says if `message` is not directly handled by the
 * receiver of the message, then it should be handled by the global fallthrough
 * receiver for that message type. If _that_ lookup fails, then we finally exit
 * with an error (`BLIMP_NO_SUCH_METHOD`).
 *
 * The code associated with each pair of symbols in the V-Table is called a
 * _method_. At its most general, a method is a C function operating on the
 * Blimp interpreter together with some data which gets passed to that function
 * each time the method is called. Since arbitrary C functions can be bound to
 * symbol pairs in the V-Table, users interacting with the V-Table directly
 * through the V-Table API (e.g. Blimp_Bind()) can cause virtually anything to
 * happen when a message is sent in a `bl:mp` program, including causing the
 * interpreter to crash, or worse. You must be careful when binding custom
 * methods in the V-Table.
 *
 * Of course, the vast majority of `bl:mp` messages are not handled by arbitrary
 * C functions. They are handled by evaluating a single BlimpExpr which was
 * bound to that method by a `bind` expression in the program. For this common
 * use case, we provide the BlimpMethod_Eval() function, which is the method
 * implementing this simple behavior. For convenience, we also provide
 * Blimp_BindExpr(), which is similar to Blimp_Bind(), but instead of binding an
 * arbitrary method, it binds BlimpMethod_Eval() together with the BlimpExpr you
 * give it.
 *
 * The V-Table API also consists of a few other special methods implementing the
 * three `bl:mp` primitive operations. These methods are automatically bound in
 * the V-Table when the interpreter is created, but they are accessible here if
 * you want to call them directly:
 *
 *    Receier Tag | Message Tag | Primitive Method
 *  --------------|-------------|-----------------------------
 *     `symbol`   |   `.get`    | BlimpMethod_PrimitiveGet()
 *     `symbol`   |    `:=`     | BlimpMethod_PrimitiveSet()
 *        _       |   `.eval`   | BlimpMethod_PrimitiveEval()
 *
 * @{
 */

/**
 * \brief A handler function for message sends.
 *
 * \param[in] blimp
 *      The interpreter.
 * \param[in] context
 *      The object in whose scope the method send is being evaluated. For sends
 *      executing in the code of a block, this is that block. For code executing
 *      at file scope, this is the global object.
 * \param[in] receiver
 *      The object receiving the method. This object's tag was used as the
 *      receiver tag when resolving this method handler in the V-Table.
 * \param[in] message
 *      The object which is being sent to the receiver. This object's tag was
 *      used as the essage tag when resolving this method handler in the
 *      V-Table.
 * \param[in] data
 *      Data which was previously associated with this method via Blimp_Bind().
 * \param[out] result
 *      A new reference.
 */
typedef BlimpStatus (*BlimpMethod)(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result);

/**
 * \brief Bind a method handler and data to a symbol pair.
 */
BlimpStatus Blimp_Bind(
    Blimp *blimp,
    const BlimpSymbol *receiver_tag,
    const BlimpSymbol *message_tag,
    BlimpMethod method,
    void *data);

/**
 * \brief Bind an expression to a symbol pair.
 *
 * When this (receiver, message) pair is resolved, the result of the message
 * will be computed by evaluating `expr` in the scope of the receiving object
 * (as opposed to the more general method of calling a bound method handler
 * function).
 *
 * Under the hood, this function uses the message handler system by binding
 * BlimpMethod_Eval() as the handler for this symbol pair, and binding `expr` as
 * the argument to be passed to BlimpMethod_Eval().
 */
BlimpStatus Blimp_BindExpr(
    Blimp *blimp,
    const BlimpSymbol *receiver_tag,
    const BlimpSymbol *message_tag,
    BlimpExpr *expr);

typedef struct {
    const char *receiver;
    const char *message;
    BlimpMethod method;
    void *data;
} BlimpVTableEntry;

typedef BlimpVTableEntry BlimpVTableFragment[];

/**
 * \brief Convenience function to bind a number of T-Table mappings at once.
 *
 * The final entry in the fragment must be all zero.
 */
BlimpStatus Blimp_BindVTableFragment(
    Blimp *blimp, BlimpVTableFragment fragment);


/**
 * \brief Find the method associated with a symbol pair.
 *
 * This function implements the three-tiered method resolution algorithm:
 *  1. First try (`receiver_tag`, `message_tag`).
 *  2. If that fails, try (`receiver_tag`, _).
 *  3. If that fails, try (_, `message_tag`).
 *
 * \par Errors
 *  * `BLIMP_NO_SUCH_METHOD`:
 *      the pair (`receiver_tag`, `message_tag`) does not having a binding in
 *      the V-Table. You must call Blimp_Bind() or Blimp_BindExpr(), or evaluate
 *      an appropriate `bind` expression, before trying to resolve the symbol
 *      pair.
 *
 */
BlimpStatus Blimp_Resolve(
    Blimp *blimp,
    const BlimpSymbol *receiver_tag,
    const BlimpSymbol *message_tag,
    BlimpMethod *method,
    void **data);

/**
 * \brief Evaluate a message send.
 *
 * This function evaluates the result of sending the given message to the given
 * receiver object, exactly as if we were evaluating a message send expression.
 *
 * This is merely a convenience wrapper around other API functions. It is
 * equivalent to calling Blimp_Resolve() to get the method handler and then
 * invoking the handler.
 */
BlimpStatus Blimp_Send(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result);

/**
 * \defgroup primitives Built-in Methods
 * @{
 */

/**
 * \brief Handle a message by evaluating the expression bound to that message.
 *
 * This method handler implements the most common behavior for message sends:
 * if `expr` was associated with the relevant (receiver, message) pair in the
 * V-Table -- either by a previous call to Blimp_BindExpr() or by evaluating
 * `bind receiver message expr` -- then `expr` is evaluated in the scope of the
 * receiver.
 */
BlimpStatus BlimpMethod_Eval(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    const BlimpExpr *body,
    BlimpObject **result);

/**
 * \brief Method handler implementing the default behavior for `symbol .get`.
 */
BlimpStatus BlimpMethod_PrimitiveGet(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result);

/**
 * \brief Method handler implementing the default behavior for `symbol :=`.
 */
BlimpStatus BlimpMethod_PrimitiveSet(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result);

/**
 * \brief Method handler implementing the default behavior for `_ .eval`.
 */
BlimpStatus BlimpMethod_PrimitiveEval(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result);

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
 *      The object in whose scope to do the evaluation. This affects, for
 *      example, the behavior of the primitive `symbol .get` and `symbol :=`
 *      operations, which work in the ambient scope.
 * \param[out] result
 *      A new reference.
 */
BlimpStatus Blimp_Eval(
    Blimp *blimp,
    const BlimpExpr *expr,
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
 * BlimpObject_ParseSymbol(), and BlimpObject_Release().
 */
BlimpStatus Blimp_EvalSymbol(
    Blimp *blimp,
    const BlimpExpr *expr,
    BlimpObject *scope,
    const BlimpSymbol **sym);

/**
 * @}
 */

#endif // BLIMP_H
