#include <dlfcn.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <blimp.h>
#include <blimp/module.h>

// Given a directory `dir` to search in and a dot-separated module path
// `qualified_module_name`, construct a file path to check for the module, by
// the following process:
//  * Separate `qualified_module_name` into a (possibly empty) prefix
//    `module_prefix` and a non-empty suffix `module_name` by splitting at the
//    last occurence of . if there is one.
//  * Replace remaining .s with / in `module_prefix` to form `module_path`.
//  * Concatenate `<dir>/<module_path>/<prefix><module_name><extension>.
//
// If successful, `*path` points to a dynamically allocated buffer containing
// the resulting string. The caller is responsible for eventually freeing the
// allocated memory by calling `free(*path)`.
static BlimpStatus MakeModulePath(
    Blimp *blimp,
    const char *dir,
    const char *prefix,
    const char *qualified_module_name,
    const char *extension,
    char **path)
{
    size_t dir_len         = strlen(dir);
    size_t module_name_len = strlen(qualified_module_name);
    size_t prefix_len      = strlen(prefix);
    size_t extension_len   = strlen(extension);

    *path = malloc(
        dir_len + 1 + module_name_len + prefix_len + extension_len + 1);
        //        /                                                '\0'
    if (*path == NULL) {
        return Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY);
    }

    // Append the directory path.
    memcpy(*path, dir, dir_len);
    (*path)[dir_len] = '/';

    // Append the qualified module name, replacing dots with slashes, up to the
    // last segment of the name, which will get appended after `prefix` to form
    // the unqualified name.
    const char *unqualified_name = qualified_module_name;
    for (size_t i = 0; i < module_name_len; ++i) {
        if (qualified_module_name[i] == '.') {
            (*path)[dir_len + 1 + i] = '/';
            unqualified_name = &qualified_module_name[i + 1];
        } else {
            (*path)[dir_len + 1 + i] = qualified_module_name[i];
        }
    }
    size_t qualified_name_len = unqualified_name - qualified_module_name;
    size_t unqualified_name_len = strlen(unqualified_name);

    // Append the prefix.
    memcpy(&(*path)[dir_len + 1 + qualified_name_len], prefix, prefix_len);

    // Append the unqualified name.
    memcpy(
        &(*path)[dir_len + 1 + qualified_name_len + prefix_len],
        unqualified_name, unqualified_name_len);

    // Append the file extension.
    memcpy(
        &(*path)[dir_len + 1 + qualified_name_len + prefix_len + unqualified_name_len],
        extension, extension_len);

    // Add a null terminator.
    (*path)[
        dir_len + 1 +
        qualified_name_len +
        prefix_len +
        unqualified_name_len +
        extension_len
    ] = '\0';

    return BLIMP_OK;
}

static BlimpStatus ImportSource(
    Blimp *blimp, BlimpObject *context, const char *path, BlimpObject **result)
{
    BlimpStatus status;

    // Parse the file.
    BlimpExpr *expr;
    if ((status = Blimp_ParseFile(blimp, path, &expr)) != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    // Evaluate the expression.
    BlimpStatus ret = Blimp_Eval(blimp, expr, context, result);
    Blimp_FreeExpr(expr);
    return ret;
}

#define _STRINGIFY(x) #x
#define STRINGIFY(x) _STRINGIFY(x)

static BlimpStatus ImportBinary(
    Blimp *blimp,
    BlimpObject *context,
    const char *module,
    const char *path,
    BlimpObject **result)
{
    void *lib = dlopen(path, RTLD_NOW|RTLD_LOCAL);
    if (lib == NULL) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "could not open extension module %s: %s", module, dlerror());
    }

    BlimpModuleInfo *info = dlsym(lib, STRINGIFY(BLIMP_MODULE_SYMBOL));
    if (info == NULL || info->magic != BLIMP_MODULE_MAGIC_NUMBER) {
        dlclose(lib);
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "the file %s does not appear to contain a bl:mp extension module",
            path);
    }

    if (info->abi_major != BLIMP_ABI_VERSION_MAJOR ||
            // The module was compiled with the wrong version of an ABI-breaking
            // change.
        info->abi_minor >  BLIMP_ABI_VERSION_MINOR
            // The module was compiled with an ABI version from the future, and
            // may rely on features that don't exist yet.
    ) {
        dlclose(lib);
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "the file %s was built for an incompatible version of bl:mp "
            "(%zu.%zu, current version is %zu.%zu)",
            path,
            (size_t)info->abi_major,         (size_t)info->abi_minor,
            (size_t)BLIMP_ABI_VERSION_MAJOR, (size_t)BLIMP_ABI_VERSION_MINOR
        );
    }

    return info->init(blimp, context, result);
}

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
    // Search the path for a file matching `path`.
    for (const char **path_entry = search_path; *path_entry; ++path_entry) {
        const char *dir = *path_entry;

        // Look for a .bli source module at this path.
        char *source_path;
        if (MakeModulePath(blimp, dir, "", module, ".bli", &source_path)
                == BLIMP_OK)
        {
            if (access(source_path, R_OK) == 0) {
                BlimpStatus ret = ImportSource(
                    blimp, context, source_path, result);
                free(source_path);
                return ret;
            }
        }
        free(source_path);

        // Look for a .so binary module at this path.
        char *binary_path;
        if (MakeModulePath(blimp, dir, "lib", module, ".so", &binary_path)
                == BLIMP_OK)
        {
            if (access(binary_path, R_OK) == 0) {
                BlimpStatus ret = ImportBinary(
                    blimp, context, module, binary_path, result);
                free(binary_path);
                return ret;
            }
        }
        free(binary_path);
    }

    return Blimp_ErrorMsg(
        blimp, BLIMP_ERROR, "could not find module `%s'", module);
}
