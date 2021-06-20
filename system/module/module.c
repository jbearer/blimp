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

// Handler for the `import` function.
static BlimpStatus ImportMethod(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    const char **path;
    if (BlimpObject_ParseExtension(
            receiver, NULL, (void **)&path) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    const BlimpSymbol *module;
    if (BlimpObject_ParseSymbol(message, &module) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return BlimpModule_Import(
        blimp, BlimpSymbol_GetName(module), context, path, result);
}

// Handler for the `import_source` function.
static BlimpStatus ImportSourceMethod(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)receiver;

    const BlimpSymbol *path;
    if (BlimpObject_ParseSymbol(message, &path) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return BlimpModule_ImportSource(
        blimp, BlimpSymbol_GetName(path), context, result);
}

// Handler for the `import_extension` function.
static BlimpStatus ImportExtensionMethod(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)receiver;

    const BlimpSymbol *path;
    if (BlimpObject_ParseSymbol(message, &path) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return BlimpModule_ImportExtension(
        blimp, BlimpSymbol_GetName(path), context, result);
}

// Handler for the `import module` macro.
static BlimpStatus ImportHandler(
    Blimp *blimp, BlimpExpr **sub_exprs, void *arg, BlimpExpr **parsed)
{
    const char **path = (const char **)arg;

    const BlimpSymbol *module;
    if (BlimpExpr_ParseSymbol(sub_exprs[1], &module) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return BlimpModule_StaticImport(
        blimp, BlimpSymbol_GetName(module), path, parsed);
}

BlimpStatus BlimpModule_Init(Blimp *blimp, const char **path)
{
    // This function binds several global names as macros and functions:
    //
    //  * The import macro -- import <symbol> -- imports a module at parse
    //    time, so that if the module defines macros, they will be used to parse
    //    the remaining input after the import statement.
    //  * The import function -- `import` <symbol> -- imports a module at
    //    runtime. This is useful, for example, when the name of the module is
    //    computed dynamically; however, it means macros defined in the module
    //    are not available at parse time.
    //  * The import_source function -- `import_source` <symbol> -- imports a
    //    source module named by its full path, skipping the search procedure
    //    performed by `import`.
    //  * The import_extension function -- `import_extension` <symbol> -- is
    //    like `import_source`, except the module named by the path must be an
    //    extension module, not a source module.

    const BlimpSymbol *import, *import_source, *import_extension, *prec3, *prec7;
    if (Blimp_GetSymbol(blimp, "import", &import) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    if (Blimp_GetSymbol(blimp, "import_source", &import_source) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    if (Blimp_GetSymbol(blimp, "import_extension", &import_extension)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    if (Blimp_GetSymbol(blimp, "3", &prec3) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    if (Blimp_GetSymbol(blimp, "7", &prec7) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    // Define the `import` function.
    BlimpObject *method;
    if (BlimpObject_NewExtension(
            blimp,
            Blimp_GlobalObject(blimp),
            (void *)path,
            ImportMethod,
            NULL,
            &method) != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    if (BlimpObject_Set(
            Blimp_GlobalObject(blimp), import, method) != BLIMP_OK)
    {
        BlimpObject_Release(method);
        return Blimp_Reraise(blimp);
    }
    BlimpObject_Release(method);

    // Define the `import_source` function.
    if (BlimpObject_NewExtension(
            blimp,
            Blimp_GlobalObject(blimp),
            NULL,
            ImportSourceMethod,
            NULL,
            &method) != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    if (BlimpObject_Set(
            Blimp_GlobalObject(blimp), import_source, method) != BLIMP_OK)
    {
        BlimpObject_Release(method);
        return Blimp_Reraise(blimp);
    }
    BlimpObject_Release(method);

    // Define the `import_extension` function.
    if (BlimpObject_NewExtension(
            blimp,
            Blimp_GlobalObject(blimp),
            NULL,
            ImportExtensionMethod,
            NULL,
            &method) != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    if (BlimpObject_Set(
            Blimp_GlobalObject(blimp), import_extension, method) != BLIMP_OK)
    {
        BlimpObject_Release(method);
        return Blimp_Reraise(blimp);
    }
    BlimpObject_Release(method);

    // Define the `import module` macro.
    BlimpGrammarSymbol macro_symbols[2] = {
        {.is_terminal=true, .symbol=import},
        {.is_terminal=false,.symbol=prec7},
    };
    if (Blimp_DefineMacro(
            blimp, prec3, macro_symbols, 2, ImportHandler, (void *)path)
        != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    return BLIMP_OK;
}

BlimpStatus BlimpModule_Import(
    Blimp *blimp,
    const char *module,
    BlimpObject *context,
    const char **path,
    BlimpObject **result)
{
    // Locate the module and parse it into an expression.
    BlimpExpr *expr;
    if (BlimpModule_StaticImport(blimp, module, path, &expr) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    // Evaluate the expression.
    BlimpStatus ret = Blimp_Eval(blimp, expr, context, result);
    Blimp_FreeExpr(expr);
    return ret;
}

BlimpStatus BlimpModule_StaticImport(
    Blimp *blimp, const char *module, const char **path, BlimpExpr **result)
{
    // Search the path for a file matching `module`.
    for (const char **path_entry = path; *path_entry; ++path_entry) {
        const char *dir = *path_entry;

        // Look for a .bli source module at this path.
        char *source_path;
        if (MakeModulePath(blimp, dir, "", module, ".bli", &source_path)
                == BLIMP_OK)
        {
            if (access(source_path, R_OK) == 0) {
                // Parse the source file at `source_path` and return the
                // resulting expression.
                BlimpStatus ret = Blimp_ParseFile(
                    blimp, source_path, result);
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
                // There's nothing we can do at parse time to import a binary
                // module, except to emit an instruction to load the module at
                // runtime. Get a symbol for the path so we can create an
                // expression reprsenting the file name.
                const BlimpSymbol *path_sym;
                if (Blimp_GetSymbol(blimp, binary_path, &path_sym)
                        != BLIMP_OK)
                {
                    free(binary_path);
                    return Blimp_Reraise(blimp);
                }
                free(binary_path);
                    // We don't need `binary_path` anymore, now that we have
                    // `path_sym`.

                // The expression we emit will be a send:
                //          import_extension binary_path
                // The `import_extension` symbol is bound to an extension object
                // which requires a full path and imports the corresponding
                // binary module.
                //
                // Before we can construct the send, we need an expression for
                // the receiver:
                const BlimpSymbol *import_extension;
                BlimpExpr *receiver;
                if (Blimp_GetSymbol(
                        blimp, "import_extension", &import_extension)
                    != BLIMP_OK)
                {
                    return Blimp_Reraise(blimp);
                }
                if (BlimpExpr_NewSymbol(blimp, import_extension, &receiver)
                        != BLIMP_OK)
                {
                    return Blimp_Reraise(blimp);
                }

                // And an expression for the message:
                BlimpExpr *message;
                if (BlimpExpr_NewSymbol(blimp, path_sym, &message)
                        != BLIMP_OK)
                {
                    Blimp_FreeExpr(receiver);
                    return Blimp_Reraise(blimp);
                }

                // Now we can construct the send:
                if (BlimpExpr_NewSend(blimp, receiver, message, result)
                        != BLIMP_OK)
                {
                    Blimp_FreeExpr(receiver);
                    Blimp_FreeExpr(message);
                    return Blimp_Reraise(blimp);
                }

                return BLIMP_OK;
            }
        }
        free(binary_path);
    }

    return Blimp_ErrorMsg(
        blimp, BLIMP_ERROR, "could not find module `%s'", module);
}

BlimpStatus BlimpModule_ImportSource(
    Blimp *blimp,
    const char *path,
    BlimpObject *context,
    BlimpObject **result)
{
    // Parse the file.
    BlimpExpr *expr;
    if (Blimp_ParseFile(blimp, path, &expr) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    // Evaluate the parsed expression.
    BlimpStatus ret = Blimp_Eval(blimp, expr, context, result);
    Blimp_FreeExpr(expr);
    return ret;
}

#define _STRINGIFY(x) #x
#define STRINGIFY(x) _STRINGIFY(x)

BlimpStatus BlimpModule_ImportExtension(
    Blimp *blimp,
    const char *path,
    BlimpObject *context,
    BlimpObject **result)
{
    void *lib = dlopen(path, RTLD_NOW|RTLD_LOCAL);
    if (lib == NULL) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "could not open extension module %s: %s", path, dlerror());
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
