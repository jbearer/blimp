#ifndef TEST_RACKET_H
#define TEST_RACKET_H

#include <stdbool.h>
#include <stdio.h>
#include <sys/types.h>

#include "options.h"

typedef struct {
    FILE *in;
    FILE *out;
    pid_t pid;
} Racket;

/**
 * Racket_Init - open a Racket interpreter.
 *
 * This function will open a Racket interpreter in a new child process, and it
 * will store information necessary for communicating with the interpreter in
 * `racket`.
 *
 * If the interpreter can be successfully started, Racket_Init will return true.
 * Afterwards, `racket` can be useed with other Racket_* functions to interact
 * with the interpreter.
 *
 * If there is an error, this function returns false, an the contents of
 * `racket` are undefined.
 */
bool Racket_Init(Racket *racket, const Options *options);

/**
 * Racket_Close - close a Racket interpreter.
 *
 * Closes a connection to a Racket interpreter which was previously opened by
 * Racket_Init. This will release resources associated with the interpreter,
 * including the child process and open files. Afterwards, `racket` may be
 * reused.
 */
void Racket_Close(Racket *racket);

/**
 * Racket_BeginCommand - obtain a file handle to send a command to Racket.
 *
 * A client can send a command to the Racket interpreter by writing the command
 * to the file stream returned by this function. When the full command has been
 * written, it can be executed by calling Racket_CommitCommand.
 */
FILE *Racket_BeginCommand(Racket *racket);

/**
 * Racket_CommitCommand - execute a command created with Racket_BeginCommand.
 *
 * This function will execute the command created using Racket_BeginCommand in
 * the Racket interpreter. If successful, it returns the result printed by the
 * interpreter. If an error occurs, it returns NULL.
 *
 * If the command takes longer than `timeout` ms to execute, execution will be
 * terminated early and the return value will be NULL.
 *
 * The memory for the resulting string is allocated dynamically, so the caller
 * should `free` that memory at some point.
 */
char *Racket_CommitCommand(Racket *racket, size_t timeout);

/**
 * Racket_Eval - evaluate the command `expr` and return the results.
 *
 * As with `Racket_CommitCommand`, this function returns non-NULL on success and
 * NULL on error. On success, the returned memory must be explicitly freed by
 * the caller.
 */
char *Racket_Eval(Racket *racket, const char *expr);

/**
 * Racket_Exec - execute a command for its side-effects in the interpreter.
 *
 * Returns true on success and false on error.
 */
bool Racket_Exec(Racket *racket, const char *expr);

#endif
