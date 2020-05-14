#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <blimp.h>

#include "module.h"
#include "options.h"

static BlimpStatus ImportMethod(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void) receiver;

    const Options *options = (const Options *)data;

    const BlimpSymbol *module;
    if (BlimpObject_ParseSymbol(message, &module) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return Module_Import(
        blimp, BlimpSymbol_GetName(module), context, options, result);
}

BlimpStatus Module_Init(Blimp *blimp, const Options *options)
{
    return Blimp_BindVTableFragment(blimp, (BlimpVTableFragment){
        {"import", "symbol", ImportMethod, (void *)options},
        {0, 0, 0, 0}
    });
}

BlimpStatus Module_Import(
    Blimp *blimp,
    const char *module,
    BlimpObject *context,
    const Options *options,
    BlimpObject **result)
{
    BlimpStatus status = BLIMP_OK;

    size_t length = strlen(module);
    char *path = malloc(length + 5);
        // Allocate space for a .bli extension plus a null byte.
    if (path == NULL) {
        status = Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY);
        goto err_malloc;
    }
    memcpy(path, module, length);

    // Replace dots with slashes.
    char *c;
    for (c = path; *c; ++c) {
        if (*c == '.') {
            *c = '/';
        }
    }

    // Add file extension and null terminator.
    memcpy(c, ".bli", 5);

    // Search the path for a file matching `path`.
    FILE *file = NULL;
    for (size_t i = 0; i < options->import_path_len; ++i) {
        const char *dir = options->import_path[i];

        int dirfd = open(dir, O_RDONLY|O_DIRECTORY);
        if (dirfd < 0) {
            continue;
        }

        int fd = openat(dirfd, path, O_RDONLY);
        close(dirfd);
        if (fd < 0) {
            continue;
        }

        file = fdopen(fd, "r");
        break;
    }
    if (file == NULL) {
        status = Blimp_ErrorMsg(
            blimp,
            BLIMP_IO_ERROR,
            "could not open module `%s'",
            module
        );
        goto err_find;
    }

    // Parse the file.
    BlimpStream *stream;
    if ((status = Blimp_OpenFileStream(
            blimp, module, file, &stream)) != BLIMP_OK)
    {
        goto err_stream;
    }
    file = NULL;

    BlimpExpr *expr;
    if ((status = Blimp_Parse(blimp, stream, &expr)) != BLIMP_OK) {
        goto err_parse;
    }

    // Evaluate the expression.
    if ((status = Blimp_Eval(blimp, expr, context, result)) != BLIMP_OK) {
        goto err_eval;
    }

err_eval:
    Blimp_FreeExpr(expr);
err_parse:
err_stream:
    if (file) fclose(file);
err_find:
    free(path);
err_malloc:
    return status;
}
