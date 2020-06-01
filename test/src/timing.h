#ifndef BLIMP_TEST_TIMING_H
#define BLIMP_TEST_TIMING_H

#include <stdint.h>
#include <time.h>

extern float cycles_per_ns;

void InitTiming(void);

static inline float CyclesPerNS(void)
{
    return cycles_per_ns;
}

static inline uint64_t RDTSC(void)
{
#if defined(__i386__) || defined(__x86_64__)
    uint32_t lo, hi;
    __asm__ volatile ("rdtsc" : "=a"(lo), "=d"(hi));
    return (uint64_t)lo | ((uint64_t)hi << 32);
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_nsec + ts.tv_sec*1000000000;
#endif
}

static inline float GetTimeNS(void)
{
    return (float)RDTSC() / CyclesPerNS();
}

#endif
