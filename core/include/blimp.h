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

typedef struct Blimp Blimp;
Blimp *Blimp_New(void);

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

typedef struct BlimpExpr BlimpExpr;

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
 */

#endif // BLIMP_H
