////////////////////////////////////////////////////////////////////////////////
// The bl:mp Module System
//
// This file defines an interface to the bl:mp module system, which allows you
// to include bl:mp files in other bl:mp programs via the built-in method
// `import symbol`.
//
// The import mechanism is built in to the interpreter, rather than
//  * libblimmp, because it is an extension; it is not part of the core language
//  * libsystem, because it is needed to load other plugins, like libsystem
//

#ifndef BLIMP_MODULE_H
#define BLIMP_MODULE_H

#include <blimp.h>
#include "options.h"

/**
 * \brief Initialize the module system in the given interpreter.
 *
 * This will bind module-system methods like `import symbol`.
 */
BlimpStatus Module_Init(Blimp *blimp, const Options *options);

/**
 * \brief Import the named module.
 *
 * The module name is converted to a file name by replacing all occurrences of .
 * in `module` with /, and appending `.bli`. For each path in
 * `options->import_path`, we check if a file with the computed name exists in
 * that directory. For the first such file we find, we parse the contents of the
 * file as a bl:mp expression and evaluate that expression in the interpreter
 * `blimp` in the scope `context`, which for most use cases should be
 * `Blimp_GlobalObject(blimp)`.
 */
BlimpStatus Module_Import(
    Blimp *blimp,
    const char *module,
    BlimpObject *context,
    const Options *options,
    BlimpObject **result);

#endif
