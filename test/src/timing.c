#include <assert.h>
#include <unistd.h>

#include "timing.h"

float cycles_per_ns;

void InitTiming(void)
{
    uint64_t start = RDTSC();
    sleep(1);
    uint64_t end = RDTSC();

    assert(end > start);

    cycles_per_ns = (float)(end - start) / 1000000000;
}
