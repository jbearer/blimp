#ifndef BLIMP_BIT_VECTOR_H
#define BLIMP_BIT_VECTOR_H

#include <stdint.h>

#include "internal/common.h"

typedef struct {
    uint64_t lo;
    uint64_t hi;
} bv128;

static const bv128 BV128_ZER0 = {0, 0};

static inline bv128 bv128_Set(bv128 bv, size_t bit)
{
    bv128 ret = bv;

    assert(bit < 128);
    if (bit < 64) {
        ret.lo |= (uint64_t)1 << bit;
    } else {
        ret.hi |= (uint64_t)1 << (bit - 64);
    }

    return ret;
}

static inline bool bv128_Get(bv128 bv, size_t bit)
{
    assert(bit < 128);
    if (bit < 64) {
        return !!(bv.lo & ((uint64_t)1 << bit));
    } else {
        return !!(bv.hi & ((uint64_t)1 << (bit - 64)));
    }
}

static inline bv128 bv128_And(bv128 bv1, bv128 bv2)
{
    return (bv128) { bv1.lo & bv2.lo, bv1.hi & bv2.hi };
}

static inline bv128 bv128_Or(bv128 bv1, bv128 bv2)
{
    return (bv128) { bv1.lo | bv2.lo, bv1.hi | bv2.hi };
}

static inline bool bv128_Test(bv128 bv)
{
    return bv.lo || bv.hi;
}

#endif
