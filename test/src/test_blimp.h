// Test-bl:mp
//
// A patched bl:mp interpreter which is capable of running .blt files. This is
// a wrapper around the core interpreter with special support for two
// primitive methods:
//      symbol{!expect|symbol}
//      block{!expect_error|_}
//

#ifndef TEST_BLIMP_H
#define TEST_BLIMP_H

#include <blimp.h>

Blimp *TestBlimp_New(const BlimpOptions *options);

#endif
