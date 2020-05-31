////////////////////////////////////////////////////////////////////////////////
// The bl:mp Module System
//
// This file defines an interface to the bl:mp module system, which allows you
// to include bl:mp files in other bl:mp programs via the built-in method
// `import symbol`.
//

#ifndef BLIMP_MODULE_H
#define BLIMP_MODULE_H

#include <blimp.h>

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
 * The module name is converted to a file name by replacing all occurrences of .
 * in `module` with /, and appending `.bli`. For each path in `import_path`, we
 * check if a file with the computed name exists in that directory. For the
 * first such file we find, we parse the contents of the file as a bl:mp
 * expression and evaluate that expression in the interpreter `blimp` in the
 * scope `context`, which for most use cases should be
 * `Blimp_GlobalObject(blimp)`.
 */
BlimpStatus BlimpModule_Import(
    Blimp *blimp,
    const char *module,
    BlimpObject *context,
    const char **path,
    BlimpObject **result);

#endif
