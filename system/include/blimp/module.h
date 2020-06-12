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

/**
 * \brief Import the named module.
 *
 * Module search is performed as follows:
 *  * For each path in `import_path`,
 *      - Check for a .bli source module by converting each occurrence of . in
 *        `module` with /, appending it to the search path, and then appending
 *        `.bli`.
 *      - If a source module exists, then evaluate the contents of the file as a
 *        `bl:mp` expression. Evaluation is performed in the scope of the object
 *        `context`, which for most purposes should be Blimp_GlobalObject().
 *        Return the results of evaluation.
 *      - If a source module does not exist in this directory, look for an
 *        \ref extensions "extension module". The module name is split into
 *        segments on `.'. All but the last segment are treated as directories
 *        and appended to the search path, intercalated with `/'. The last
 *        segment is used to derive the name of a bl:mp extension shared object
 *        module: lib<segment>.so.
 *      - If the resulting file path exists, it is loaded into the program using
 *        the OS loader (e.g. dlopen). The BlimpModuleInfo structure describing
 *        the module is located using the symbol `_blimp_module_info`, and the
 *        module's `init` function is called.
 *  * If no matching module is found, an error is raised and the result is
 *    undefined.
 */
BlimpStatus BlimpModule_Import(
    Blimp *blimp,
    const char *module,
    BlimpObject *context,
    const char **path,
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
#define BLIMP_ABI_VERSION_MINOR 1
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
