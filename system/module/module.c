#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <blimp.h>
#include <blimp/module.h>

static BlimpStatus ImportMethod(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void) receiver;

    const char **path = (const char **)data;

    const BlimpSymbol *module;
    if (BlimpObject_ParseSymbol(message, &module) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return BlimpModule_Import(
        blimp, BlimpSymbol_GetName(module), context, path, result);
}

BlimpStatus BlimpModule_Init(Blimp *blimp, const char **path)
{
    return Blimp_BindVTableFragment(blimp, (BlimpVTableFragment){
        {"import", "symbol", ImportMethod, (void *)path},
        {0, 0, 0, 0}
    });
}

BlimpStatus BlimpModule_Import(
    Blimp *blimp,
    const char *module,
    BlimpObject *context,
    const char **search_path,
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
    memcpy(path, module, length+1);

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
    for (const char **path_entry = search_path; *path_entry; ++path_entry) {
        const char *dir = *path_entry;

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
