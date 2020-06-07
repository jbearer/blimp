////////////////////////////////////////////////////////////////////////////////
// BlimpOS
//
// A cross-platform interface to platform specific functionality.
//
// This module implements functions which cannot be expressed portably by
// providing different implementations for each platform on which they are
// supported. There are two kinds of platform-specific functions:
//  * Optional functions are always present, and will fail at runtime with
//    `BLIMP_NOT_SUPPORTED` if they are not implemented for the current
//    platform.
//  * Required functions will cause a build error if they cannot be implemented
//    for the target platform. They will never return `BLIMP_NOT_SUPPORTED` at
//    runtime.
//
// All non-portable functionality should be implemented in os.c with an
// appropriate, portable interface here. All code in blimpcore outside of this
// module should be portable.

#ifndef BLIMP_OS_H
#define BLIMP_OS_H

#include <stdint.h>

#include "internal/common.h"

// Get the lowest and highest addresses in the calling thread's stack, and the
// direction of stack growth.
//
// This functionality is optional.
PRIVATE Status OS_GetStackLimits(
    Blimp *blimp, uint8_t **lo, uint8_t **hi, bool *grows_down);

#endif
