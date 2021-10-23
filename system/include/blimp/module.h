////////////////////////////////////////////////////////////////////////////////
// The bl:mp Module System
//
// This file defines an interface to the bl:mp module system, which allows you
// to include bl:mp files in other bl:mp programs via the built-in method
// `import symbol`.
//

#ifndef BLIMP_MODULE_H
#define BLIMP_MODULE_H

#include <stdint.h>

#include <blimp.h>

/**
 * \defgroup importing Importing Modules
 *
 * @{
 */

/**
 * \brief Initialize the module system in the given interpreter.
 *
 * \param path  NULL-terminated array of file paths to search for modules.
 *
 * This will bind module-system methods like `import symbol`.
 */
BlimpStatus BlimpModule_Init(Blimp *blimp, const char **path);

typedef enum {
    BLIMP_MODULE_TEXT = 0x1,
    BLIMP_MODULE_BINARY = 0x2,
} BlimpModuleType;

/**
 * \brief Find the full path of the module named `module`.
 *
 * \param[in] module
 *      The module to search for.
 * \param[in] path
 *      The search path. Must be a NULL-terminated array of strings.
 * \param[in,out] type
 *      The type of module to search for (text or binary). Must be a bitwise or
 *      of zero or more BlimpModuleType values. Upon succesful return, this
 *      value is modified to indicate the type of the discovered module. The
 *      resulting value will be exactly one BlimpModuleType value.
 * \param[out] ret
 *      The full path to the file containing the discovered module.
 *
 * Module search is performed as follows:
 *  * For each path in `path`,
 *      - Check for a .bli source module by converting each occurrence of . in
 *        `module` with /, appending it to the search path, and then appending
 *        `.bli`. If one exists, return it.
 *      - If a source module does not exist in this directory, look for an
 *        \ref extensions "extension module". The module name is split into
 *        segments on `.'. All but the last segment are treated as directories
 *        and appended to the search path, intercalated with `/'. The last
 *        segment is used to derive the name of a bl:mp extension shared object
 *        module: lib<segment>.so. If an extension module path exists, return
 *        it.
 *  * If no matching module is found, an error is raised and the result is
 *    undefined.
 */
BlimpStatus BlimpModule_Search(
    Blimp *blimp,
    const char *module,
    const char **path,
    BlimpModuleType *type,
    const char **ret);

/**
 * \brief Import the named module.
 *
 * Module search is performed as follows:
 *  * Find a bl:mp source module or extension module as if by
 *
 *        BlimpModule_Search(
 *            blimp, module, path, &(BLIMP_MODULE_TEXT|BLIMP_MODULE_BINARY),
 *            &module_path)
 *
 *  * If no matching module is found, an error is raised and the result is
 *    undefined.
 *  * Otherwise,
 *      - If the module is a source module, evaluate the contents of the file as
 *        a `bl:mp` expression. Evaluation is performed in the scope of the
 *        object `context`, which for most purposes should be
 *        Blimp_GlobalObject(). Return the results of evaluation.
 *      - If the module is a binary module, it is loaded into the program using
 *        the OS loader (e.g. dlopen). The BlimpModuleInfo structure describing
 *        the module is located using the symbol `_blimp_module_info`, and the
 *        module's `init` function is called.
 */
BlimpStatus BlimpModule_Import(
    Blimp *blimp,
    const char *module,
    BlimpObject *context,
    const char **path,
    BlimpNonTerminal nt,
    BlimpObject **result);

/**
 * \brief Import, but do not evaluate, the named module.
 *
 * This function imports `module` exactly like BlimpModule_Import(), but instead
 * of evaluating the imported module, it simply parses it and returns the
 * resulting parse tree.
 */
BlimpStatus BlimpModule_StaticImport(
    Blimp *blimp,
    const char *module,
    const char **path,
    BlimpNonTerminal nt,
    BlimpParseTree **result);

/**
 * \brief Import the source module located at `path`.
 *
 * This function is like BlimpModule_Import(), except it does not attempt to
 * find the module based on the module search path. Instead, `path` must be a
 * full path (absolute or relative) to a bl:mp source module.
 */
BlimpStatus BlimpModule_ImportSource(
    Blimp *blimp,
    const char *path,
    BlimpNonTerminal nt,
    BlimpObject *context,
    BlimpObject **result);

/**
 * \brief Import the extension module located at `path`.
 *
 * This function is like BlimpModule_Import(), except it does not attempt to
 * find the module based on the module search path. Instead, `path` must be a
 * full path (absolute or relative) to a bl:mp extension module.
 */
BlimpStatus BlimpModule_ImportExtension(
    Blimp *blimp,
    const char *path,
    BlimpObject *context,
    BlimpObject **result);

/**
 * @}
 */

/**
 * \defgroup extensions Extension Modules
 *
 * Extension modules provide a way to add additional functionality to `bl:mp` by
 * exposing functions written in C to `bl:mp` programs.
 *
 * An extension module is just a shared object file which exports a special
 * symbol containing information used by the `bl:mp` loader to load the module
 * at runtime.
 *
 * The most important piece of information exported by an extension module is
 * its `init` function. This is the entry point to the module. Each time the
 * module is imported by a `bl:mp` program, the loader calls the module's `init`
 * function with a reference to the `bl:mp` interpreter that is loading the
 * module. There, the module can use the extent of the `bl:mp` API to do
 * whatever it wants to the interpreter state: for example, binding methods,
 * creating global variables, or sending messages to objects.
 *
 * @{
 */

#define BLIMP_MODULE_MAGIC_NUMBER 0xb02a

#define BLIMP_ABI_VERSION_MAJOR 0
    ///< Changes that break backwards compatibility (e.g. removal of features
    ///  which old modules might rely on). Precompiled extension modules can
    ///  only be loaded by interpreters with the same major version.
#define BLIMP_ABI_VERSION_MINOR 2
    ///< Changes that break forward compatibility (e.g. introduction of features
    ///  that new modules might rely on). Precompiled extension modules can only
    ///  be loaded by interpreters with the same or a newer minor version.
#define BLIMP_ABI_VERSION_PATCH 0
    ///< Changes that do not affect compatibility (e.g. new optimizations, or
    ///  optional features which are guarded by a version check). The patch
    ///  version does not affect loadability of extension modules.

typedef struct {
    /// Magic number so we can sanity check that this is a bl:mp module. This
    /// should always match BLIMP_MODULE_MAGIC_NUMBER.
    uint16_t magic;

    /// The ABI version this module was compiled for.
    uint8_t abi_major;
    uint8_t abi_minor;
    uint8_t abi_patch;

    /// Initialization function, called when the module is imported.
    BlimpStatus(*init)(
        Blimp *blimp, BlimpObject *context, BlimpObject **result);
} BlimpModuleInfo;

#define BLIMP_MODULE_SYMBOL _blimp_module_info
    // The standard symbol exported by extension modules. This is where the
    // loader looks to find the BlimpModuleInfo struct.

/**
 * \brief Define a `bl:mp` extension module.
 *
 * This macro must be included once in any shared library which is to be used as
 * a `bl:mp` extension module. It defines the symbol `_blimp_module_info`, which
 * points to a BlimpModuleInfo struct. The version fields of the struct are
 * filled in to match the ABI version which was distributed with this header.
 * The initialization function is set to the given function, which will be
 * called whenever the extension module is imported.
 */
#define BLIMP_MODULE(INIT) \
    BlimpModuleInfo BLIMP_MODULE_SYMBOL = { \
        .magic     = BLIMP_MODULE_MAGIC_NUMBER, \
        .abi_major = BLIMP_ABI_VERSION_MAJOR, \
        .abi_minor = BLIMP_ABI_VERSION_MINOR, \
        .abi_patch = BLIMP_ABI_VERSION_PATCH, \
        .init = INIT, \
    }

/**
 * @}
 */

#endif
