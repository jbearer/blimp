#include "internal/random.h"

static inline uint64_t MixSeed(uint64_t seed)
{
    seed = (seed ^ (seed >> 33)) * 0xff51afd7ed558ccd;
    seed = (seed ^ (seed >> 33)) * 0xc4ceb9fe1a85ec53;
    seed = (seed ^ (seed >> 33));
    return seed;
}

static size_t PopCount(uint64_t word)
{
#if __GNUC__
    return __builtin_popcountll(word);
#else
    size_t count = 0;
    for (size_t i = 0; i < 64; ++i) {
        if (word & (1ull<<i)) {
            ++count;
        }
    }
    return count;
#endif
}

void Random_Init(Random *rand, uint64_t seed)
{
    rand->seed = MixSeed(seed);

    uint64_t g = seed + 0x9e3779b97f4a7c15;
    g = (g ^ (g >> 30)) * 0xbf58476d1ce4e5b9;
    g = (g ^ (g >> 27)) * 0x94d049bb133111eb;
    g = (g ^ (g >> 31));
    g = g | 1;

    if (PopCount(g ^ (g >> 1)) >= 24) {
        rand->gamma = g;
    } else {
        rand->gamma = g ^ 0xaaaaaaaaaaaaaaaa;
    }
}

uint64_t Random_NextWord(Random *rand)
{
    rand->seed += rand->gamma;
    return MixSeed(rand->seed);
}
