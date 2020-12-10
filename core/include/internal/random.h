// The pseudo-random number generator we use here is based on the SplitMix
// algorithm, which is known to provide a good tradeoff between uniformity
// and efficiency. It is used by such venerable projects as the JDK.


#ifndef BLIMP_RANDOM_H
#define BLIMP_RANDOM_H

#include <stdint.h>

#include "common.h"

typedef struct {
    uint64_t seed;
    uint64_t gamma;
} Random;

PRIVATE void Random_Init(Random *rand, uint64_t seed);
PRIVATE uint64_t Random_NextWord(Random *rand);

#endif
