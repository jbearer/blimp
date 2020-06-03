#include <math.h>

#include "options.h"
#include "test_blimp.h"
#include "timing.h"

static inline void VoidReturn(Blimp *blimp, BlimpObject **result)
{
    BlimpObject_Borrow(Blimp_GlobalObject(blimp));
    *result = Blimp_GlobalObject(blimp);
}

static inline bool ParseUIntSymbol(const BlimpSymbol *sym, size_t *result)
{
    char *invalid_char;
    *result = strtol(BlimpSymbol_GetName(sym), &invalid_char, 0);
    return !*invalid_char;
}

static inline BlimpStatus MakeUIntSymbol(
    Blimp *blimp, size_t n, const BlimpSymbol **sym)
{
    char buf[32];
    snprintf(buf, sizeof(buf), "%zu", n);
    return Blimp_GetSymbol(blimp, buf, sym);
}

static BlimpStatus Expect(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)scope;
    (void)data;

    const BlimpSymbol *rec_sym;
    if (BlimpObject_ParseSymbol(receiver, &rec_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    BlimpObject *arg;
    if (BlimpObject_Eval(message, &arg) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    const BlimpSymbol *msg_sym;
    if (BlimpObject_ParseSymbol(arg, &msg_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    if (rec_sym != msg_sym) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "expected `%s' to match `%s'",
            BlimpSymbol_GetName(rec_sym),
            BlimpSymbol_GetName(msg_sym)
        );
    }

    BlimpObject_Borrow(receiver);
    *result = receiver;

    return BLIMP_OK;
}

static BlimpStatus ExpectLT(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)scope;
    (void)data;

    const BlimpSymbol *rec_sym;
    if (BlimpObject_ParseSymbol(receiver, &rec_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    size_t rec;
    if (!ParseUIntSymbol(rec_sym, &rec)) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "receiver of !expect_lt must be a numeric symbol (got `%s')",
            BlimpSymbol_GetName(rec_sym));
    }

    BlimpObject *arg;
    if (BlimpObject_Eval(message, &arg) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    const BlimpSymbol *msg_sym;
    if (BlimpObject_ParseSymbol(arg, &msg_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    size_t msg;
    if (!ParseUIntSymbol(msg_sym, &msg)) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "body of !expect_lt must be a numeric symbol (got `%s')",
            BlimpSymbol_GetName(msg_sym));
    }

    if (rec >= msg) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "expected `%zu' to be less than `%zu'", rec, msg);
    }

    BlimpObject_Borrow(receiver);
    *result = receiver;

    return BLIMP_OK;
}

static BlimpStatus ExpectPercent(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)scope;
    (void)data;

    const BlimpSymbol *rec_sym;
    if (BlimpObject_ParseSymbol(receiver, &rec_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    size_t actual;
    if (!ParseUIntSymbol(rec_sym, &actual)) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "receiver of !expect_percent must be a numeric symbol (got `%s')",
            BlimpSymbol_GetName(rec_sym));
    }

    BlimpObject *arg;
    if (BlimpObject_Eval(message, &arg) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    const BlimpSymbol *percent_sym;
    if (BlimpObject_ParseBlock(arg, &percent_sym, NULL) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    size_t percent;
    if (!ParseUIntSymbol(percent_sym, &percent)) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "tag of !expect_percent body must be a numeric symbol (got `%s')",
            BlimpSymbol_GetName(percent_sym));
    }

    BlimpObject *expected_obj;
    if (BlimpObject_Eval(arg, &expected_obj) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    const BlimpSymbol *expected_sym;
    if (BlimpObject_ParseSymbol(expected_obj, &expected_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    size_t expected;
    if (!ParseUIntSymbol(expected_sym, &expected)) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "body of !expect_percent body must be a numeric symbol (got `%s')",
            BlimpSymbol_GetName(expected_sym));
    }

    if (((float)actual - (float)expected)/expected > (float)percent/100) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "expected `%zu' to match `%zu' within %zu",
            actual, expected, percent
        );
    }

    BlimpObject_Borrow(receiver);
    *result = receiver;

    return BLIMP_OK;
}

static BlimpStatus ExpectError(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)scope;
    (void)message;
    (void)data;

    if (BlimpObject_Eval(receiver, result) == BLIMP_OK) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR, "expected error");
    }

    VoidReturn(blimp, result);
    return BLIMP_OK;
}

static void FormatNS(float *time, const char **unit)
{
    if (*time >= 1000000000) {
        *time /= 1000000000;
        *unit = "s";
    } else if (*time >= 1000000) {
        *time /= 1000000;
        *unit = "ms";
    } else if (*time >= 1000) {
        *time /= 1000;
        *unit = "us";
    } else {
        *unit = "ns";
    }
}

static BlimpStatus Benchmark(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)scope;

    const Test    *test    = data;
    const Options *options = &test->options;

    size_t iter;
    size_t ops;
    size_t warmup;

    // Get the name of the benchmark.
    const BlimpSymbol *name;
    if (BlimpObject_ParseBlock(message, &name, NULL) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    // Evaluate the receiver for the side effects of setting up the `n`, `ops`,
    // and `warmup` member variables.
    if (BlimpObject_Eval(receiver, result) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    BlimpObject_Release(*result);

    // Get the number of iterations to run.
    const BlimpSymbol *iter_key;
    if (Blimp_GetSymbol(blimp, "n", &iter_key) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    BlimpObject *iter_obj;
    const BlimpSymbol *iter_sym;
    if (BlimpObject_Get(receiver, iter_key, &iter_obj) != BLIMP_OK ||
        BlimpObject_ParseSymbol(iter_obj, &iter_sym) != BLIMP_OK ||
        !ParseUIntSymbol(iter_sym, &iter))
    {
        iter = 1000;
            // Do 1000 iterations by default.
    }

    // Clean up our reference to `n`.
    BlimpObject_Set(receiver, iter_key, receiver);

    // Get the number of operations per iteration.
    const BlimpSymbol *ops_key;
    if (Blimp_GetSymbol(blimp, "ops", &ops_key) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    BlimpObject *ops_obj;
    const BlimpSymbol *ops_sym;
    if (BlimpObject_Get(receiver, ops_key, &ops_obj) != BLIMP_OK ||
        BlimpObject_ParseSymbol(ops_obj, &ops_sym) != BLIMP_OK ||
        !ParseUIntSymbol(ops_sym, &ops))
    {
        ops = 1;
            // By default, each iteration is considered one operation.
    }

    // Clean up our reference to `ops`.
    BlimpObject_Set(receiver, ops_key, receiver);

    // Get the number of warmup iterations.
    const BlimpSymbol *warmup_key;
    if (Blimp_GetSymbol(blimp, "warmup", &warmup_key) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    BlimpObject *warmup_obj;
    const BlimpSymbol *warmup_sym;
    if (BlimpObject_Get(receiver, warmup_key, &warmup_obj) != BLIMP_OK ||
        BlimpObject_ParseSymbol(warmup_obj, &warmup_sym) != BLIMP_OK ||
        !ParseUIntSymbol(warmup_sym, &warmup))
    {
        warmup = 0;
            // No warmup by default.
    }

    // Clean up our reference to `warmup`.
    BlimpObject_Set(receiver, warmup_key, receiver);

    // Do warmup
    for (size_t i = 0; i < warmup; ++i) {
        if (BlimpObject_Eval(message, result) != BLIMP_OK) {
            return Blimp_Reraise(blimp);
        }
        BlimpObject_Release(*result);
    }

    // Set up stats.
    size_t min = SIZE_MAX;
    size_t max = 0;
    size_t total = 0;
    float sse = 0;
        // Online computation of the sum squared error. This can be used after
        // the data is collected to compute variance and standard deviation.
    float avg = 0;
        // Running estimate of the average. Used to compute `sse` online.
    float cycles_per_ns = CyclesPerNS();

    // Run the benchmark;
    for (size_t i = 0; i < iter; ++i) {
        uint64_t start = RDTSC();

        if (BlimpObject_Eval(message, result) != BLIMP_OK) {
            return Blimp_Reraise(blimp);
        }

        BlimpObject_Release(*result);

        uint64_t elapsed = RDTSC() - start;
        if (elapsed < min) {
            min = elapsed;
        }
        if (elapsed > max) {
            max = elapsed;
        }
        total += elapsed;

        float old_avg = avg;
        avg += ((float)elapsed/ops - old_avg)/(i+1);
        sse += ((float)elapsed/ops - old_avg)*((float)elapsed/ops - avg);
    }

    float std_dev;
    if (iter <= 1) {
        std_dev = 0;
    } else {
        std_dev = sqrt(sse/(iter - 1))/cycles_per_ns;
    }

    float avg_time = (float)total/(iter*ops)/cycles_per_ns;
    float min_time = (float)min/ops/cycles_per_ns;
    float max_time = (float)max/ops/cycles_per_ns;

    // Write the results to a file.
    if (options->perf_report) {
        fprintf(options->perf_report, "\"%s\",\"%s\",\"%s\",%zu,%zu,%f,%f,%f,%f,%f\n",
            test->group->name, test->name, BlimpSymbol_GetName(name),
            iter, ops, cycles_per_ns, avg_time, std_dev, min_time, max_time);
    }

    // Write results to the terminal.

    const char *avg_unit;
    FormatNS(&avg_time, &avg_unit);

    const char *std_dev_unit;
    FormatNS(&std_dev, &std_dev_unit);

    const char *min_unit;
    FormatNS(&min_time, &min_unit);

    const char *max_unit;
    FormatNS(&max_time, &max_unit);

    if (options->verbosity >= VERB_STATS) {
        printf("Benchmark results for `%s':\n", BlimpSymbol_GetName(name));
        printf("  Iterations: %zu\n", iter);
        printf("  Cycles/ns:  %.2f\n", cycles_per_ns);
        printf("  Operations: %zu\n", iter*ops);
        printf("  Total time: %.3fs\n", (float)total/1000000000);
        printf("  Time/Op:    %.3f%s +- %.3f%s\n", avg_time, avg_unit, std_dev, std_dev_unit);
        printf("  Min:        %.3f%s\n", min_time, min_unit);
        printf("  Max:        %.3f%s\n", max_time, max_unit);
    }


    VoidReturn(blimp, result);
    return BLIMP_OK;
}

void PrintPerfReportHeader(FILE *perf_report)
{
    fprintf(perf_report, "group,test,benchmark,iterations,ops/iteration,cycles/ns,avg,min,max\n");
}

static BlimpStatus GC_Allocated(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)receiver;
    (void)message;
    (void)data;

    const BlimpSymbol *sym;
    if (MakeUIntSymbol(blimp, Blimp_GetGCStatistics(blimp).allocated, &sym)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    return BlimpObject_NewSymbol(blimp, scope, sym, result);
}

static BlimpStatus GC_Reachable(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)receiver;
    (void)message;
    (void)data;

    const BlimpSymbol *sym;
    if (MakeUIntSymbol(blimp, Blimp_GetGCStatistics(blimp).reachable, &sym)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    return BlimpObject_NewSymbol(blimp, scope, sym, result);
}

static BlimpStatus GC_Unreachable(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)receiver;
    (void)message;
    (void)data;

    BlimpGCStatistics stats = Blimp_GetGCStatistics(blimp);
    size_t unreachable = stats.allocated - stats.reachable;

    const BlimpSymbol *sym;
    if (MakeUIntSymbol(blimp, unreachable, &sym)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    return BlimpObject_NewSymbol(blimp, scope, sym, result);
}

static BlimpStatus GC_HighWaterMark(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)receiver;
    (void)message;
    (void)data;

    const BlimpSymbol *sym;
    if (MakeUIntSymbol(blimp, Blimp_GetGCStatistics(blimp).max_allocated, &sym)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    return BlimpObject_NewSymbol(blimp, scope, sym, result);
}

static BlimpStatus GC_ExpectClean(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)scope;
    (void)receiver;
    (void)message;
    (void)data;

    BlimpGCStatistics stats = Blimp_GetGCStatistics(blimp);

    // All allocated objects should be reachable.
    if (stats.reachable != stats.allocated) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "expected a clean heap, but found %zu unreachable objects",
            stats.allocated - stats.reachable);
    }

    VoidReturn(blimp, result);
    return BLIMP_OK;
}

static BlimpStatus GC_CheckCollect(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)scope;
    (void)receiver;
    (void)message;
    (void)data;

    Blimp_CollectGarbage(blimp);
    BlimpGCStatistics stats = Blimp_GetGCStatistics(blimp);

    // All allocated objects should be reachable.
    if (stats.reachable != stats.allocated) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "expected a clean heap, but found %zu unreachable objects",
            stats.allocated - stats.reachable);
    }

    VoidReturn(blimp, result);
    return BLIMP_OK;
}

static BlimpStatus GC_Collect(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)scope;
    (void)receiver;
    (void)message;
    (void)data;

    Blimp_CollectGarbage(blimp);

    VoidReturn(blimp, result);
    return BLIMP_OK;
}

static BlimpStatus GC_PrintStats(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)scope;
    (void)receiver;
    (void)message;

    const Test    *test    = data;
    const Options *options = &test->options;

    VoidReturn(blimp, result);

    if (options->verbosity < VERB_STATS) {
        return BLIMP_OK;
    }

    BlimpGCStatistics stats = Blimp_GetGCStatistics(blimp);
    printf("GC Statistics:\n");
    printf("  created:         %zu\n", stats.created);
    printf("  allocated now:   %zu\n", stats.allocated);
    printf("  reachable:       %zu\n", stats.reachable);
    printf("  high water mark: %zu\n", stats.max_allocated);
    printf("  # of clumps:     %zu\n", stats.clumps);
    if (stats.clumps > 0) {
        printf("  clump avg:       %.1f\n", (float)stats.entangled/stats.clumps);
        printf("  clump max:       %zu\n", stats.max_clump);
        printf("  clump min:       %zu\n", stats.min_clump);
    }
    printf("  # of collections: %zu\n", stats.collections);
    return BLIMP_OK;
}

Blimp *TestBlimp_New(Test *test)
{
    Blimp *blimp = Blimp_New(&test->options.blimp_options);
    if (blimp == NULL) {
        return blimp;
    }

    BlimpVTableFragment table = {
        { "symbol", "!expect",          Expect,           test },
        { "symbol", "!expect_lt",       ExpectLT,         test },
        { "symbol", "!expect_percent",  ExpectPercent,    test },
        { "_",      "!expect_error",    ExpectError,      test },
        { "!benchmark","_",             Benchmark,        test },
        { "gc",     "!allocated",       GC_Allocated,     test },
        { "gc",     "!reachable",       GC_Reachable,     test },
        { "gc",     "!unreachable",     GC_Unreachable,   test },
        { "gc",     "!high_water_mark", GC_HighWaterMark, test },
        { "gc",     "!collect",         GC_Collect,       test },
        { "gc",     "!expect_clean",    GC_ExpectClean,   test },
        { "gc",     "!check_collect",   GC_CheckCollect,  test },
        { "gc",     "!print_stats",     GC_PrintStats,    test },
        { 0, 0, 0, 0}
    };
    if (Blimp_BindVTableFragment(blimp, table) != BLIMP_OK) {
        Blimp_Delete(blimp);
        return NULL;
    }

    return blimp;
}
