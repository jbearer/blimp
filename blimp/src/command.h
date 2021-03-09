////////////////////////////////////////////////////////////////////////////////
// Commands for the interactive interpreter
//

#ifndef BLIMP_COMMAND_H
#define BLIMP_COMMAND_H

#include <blimp.h>

#include "debug.h"

BlimpStatus InitCommands(Blimp *blimp);
BlimpStatus InitDebuggerCommands(Blimp *blimp, Debugger *db);

#endif
