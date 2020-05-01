#define _GNU_SOURCE

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <unistd.h>

#include "racket.h"

#define RACKET_PROMPT "> "
#define RACKET_PROMPT_LEN (sizeof(RACKET_PROMPT) - 1)

static inline bool WouldBlock(int err)
{
    return err == EWOULDBLOCK || err == EAGAIN;
        // Annoyingly, POSIX allows non-blocking I/O functions to return either
        // of these two error codes when a call would block, so we have to check
        // for both.
}

// This function provides an API similar to the POSIX function `getline`, except
// that it reads until the next Racket prompt ("> ") or EOF, instead of newline
// or EOF. It consumes the prompt, if one is found.
static bool ReadUntilPrompt(
    Racket *racket, char **line, size_t *n, size_t timeout_ms)
{
    FILE *f = racket->out;
    size_t len = 0;
        // The length of the result string. The capacity of the dynamically
        // allocated buffer will be stored in `*n`, and will always be at least
        // as long as `len`.

    if (*line == NULL) {
        // Initialize `line` and `n` to reasonable values.
        *n = 80;
        *line = malloc(*n);
        if (!*line) {
            return false;
        }
    }

    bool timed_out = false;
    while (true) {
        int c;

        // Read as many characters as we can until we hit the end of the file or
        // until we would block waiting for data.
        while ((c = fgetc(f)) != EOF) {
            // Append a character to the result.
            if (len >= *n) {
                *n *= 2;
                *line = realloc(*line, *n);
            }
            (*line)[len++] = c;

            // If the result is long enough to end with a prompt, check if it
            // ends with a prompt.
            if (len >= RACKET_PROMPT_LEN &&
                strncmp(
                    *line + len - RACKET_PROMPT_LEN,
                    RACKET_PROMPT,
                    RACKET_PROMPT_LEN
                ) == 0)
            {
                // Discard the prompt from the result.
                len -= RACKET_PROMPT_LEN;
                (*line)[len] = '\0';

                return true;
            }
        }
        if (feof(f)) {
            if (len) {
                // If we reached the end of the file and got data, return it.
                return true;
            } else {
                errno = ENODATA;
                break;
            }
        }
        if (!WouldBlock(errno)) {
            // We failed for some other reason than being blocked.
            break;
        }
        clearerr(f);

        // Wait for `timeout_ms` for more data to arrive on the pipe.
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(fileno(f), &fds);
        struct timeval timeout = {
            .tv_sec  = timeout_ms/1000,
            .tv_usec = (timeout_ms%1000)*1000,
        };
        int ret = select(fileno(f) + 1, &fds, NULL, NULL, &timeout) > 0;
        if (ret == 0) {
            if (!timed_out) {
                timed_out = true;

                // Send SIGINT to Racket when we time out, to break it out of
                // whatever loop it's stuck in.
                kill(racket->pid, SIGINT);

                // Keep going. Hopefully within a short time Racket will handle
                // the signal, stop what it's doing, and give us a prompt. Since
                // we've set the timed_out flag, we will wait exactly one more
                // time for Racket to show us a prompt.
            } else {
                // Otherwise, if we've already timed out and signalled, and we
                // still aren't getting output, just break.
                errno = ETIMEDOUT;
                break;
            }
        } else if (ret < 0) {
            break;
        }
    }

    free(*line);
    *line = NULL;
    return false;
}

bool Racket_Init(Racket *racket, const Options *options)
{
    signal(SIGPIPE, SIG_IGN);

    // Set up two pipes to the Racket interpreter, one for input and one for
    // output. We will create the output pipe in non-blocking mode, which allows
    // us to wait for output with a timeout. This lets the test suite fail
    // gracefully if a test program hangs the Racket interpreter.
    int racket_in[2], racket_out[2];
    if (pipe(racket_in) < 0 || pipe2(racket_out, O_NONBLOCK) < 0) {
        return false;
    }

    pid_t parent_pid = getpid();
    if ((racket->pid = fork()) < 0) {
        return false;
    } else if (racket->pid) {
        // Parent process. Close read end of input and write end of output, and
        // return the still-open pipe ends.
        close(racket_in[0]);
        close(racket_out[1]);

        racket->in = fdopen(racket_in[1],  "w");
        racket->out = fdopen(racket_out[0], "r");
        if (!racket->in || !racket->out) {
            close(racket_in[1]);
            close(racket_out[0]);
            return false;
        }

        // Wait for a prompt.
        char *line = NULL;
        size_t n = 0;
        if (!ReadUntilPrompt(racket, &line, &n, 3000)) {
            return false;
        }
        free(line);

    } else {
        // Redirect stdin to the read end of the input pipe.
        if (dup2(racket_in[0], STDIN_FILENO) < 0) {
            if (options->verbosity >= VERB_DEBUG) {
                fprintf(stderr, "racket: failed to redirect stdin: %s\n",
                    strerror(errno));
            }
            exit(1);
        }

        // Redirect stdout to the write end of the output pipe.
        if (dup2(racket_out[1], STDOUT_FILENO) < 0) {
            if (options->verbosity >= VERB_DEBUG) {
                fprintf(stderr, "racket: failed to redirect stdout: %s\n",
                    strerror(errno));
            }
            exit(1);
        }

        // Close the unused pipe ends.
        close(racket_in[1]);
        close(racket_out[0]);

        // Kill the child if the parent dies first.
        if (prctl(PR_SET_PDEATHSIG, SIGTERM) < 0) {
            if (options->verbosity >= VERB_DEBUG) {
                fprintf(stderr, "racket: failed to start: %s\n",
                    strerror(errno));
            }
            exit(1);
        }

        // It's possible (though unlikely) that the parent had already died
        // before we isntalled the SIGTERM signal. Now that we have that signal
        // set up, we are guaranteed to catch any future parent death. So we
        // just need to check once if the parent is already dead.
        if (getppid() != parent_pid) {
            if (options->verbosity >= VERB_DEBUG) {
                fprintf(stderr, "racket: exiting: detected parent death\n");
            }
            exit(1);
        }

        if (options->verbosity < VERB_DEBUG) {
            // Redirect stderr to nowhere.
            close(STDERR_FILENO);
            open("/dev/null", O_WRONLY);
        }

        // Execute racket.
        if (execlp("racket", "-i", NULL) < 0) {
            if (options->verbosity >= VERB_DEBUG) {
                fprintf(stderr, "racket: failed to start: %s\n",
                    strerror(errno));
            }
            exit(1);
        }
    }

    return true;
}

void Racket_Close(Racket *racket)
{
    fclose(racket->in);
    fclose(racket->out);
}

FILE *Racket_BeginCommand(Racket *racket)
{
    return racket->in;
}

char *Racket_CommitCommand(Racket *racket, size_t timeout_ms)
{
    fputc('\n', racket->in);
    fflush(racket->in);

    char *output = NULL;
    size_t output_size = 0;
    if (ReadUntilPrompt(racket, &output, &output_size, timeout_ms)) {
        return output;
    } else {
        return NULL;
    }
}

char *Racket_Eval(Racket *racket, const char *expr)
{
    FILE *f = Racket_BeginCommand(racket);
    fputs(expr, f);
    return Racket_CommitCommand(racket, 5000);
}

bool Racket_Exec(Racket *racket, const char *expr)
{
    char *output = Racket_Eval(racket, expr);
    free(output);
    return output != NULL;
}
