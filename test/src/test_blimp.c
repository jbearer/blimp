#include <assert.h>
#include <math.h>
#include <string.h>

#include "command.h"
#include "options.h"
#include "test_blimp.h"
#include "timing.h"

#define MAX_ARGUMENTS 4

static inline BlimpStatus VoidReturn(Blimp *blimp, BlimpObject **result)
{
    BlimpObject_Borrow(Blimp_GlobalObject(blimp));
    *result = Blimp_GlobalObject(blimp);
    return BLIMP_OK;
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
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    (void)test;

    const BlimpSymbol *sym1;
    if (BlimpObject_ParseSymbol(args[0], &sym1) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    const BlimpSymbol *sym2;
    if (Blimp_GetSymbol(blimp, "true", &sym2) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    if (sym1 != sym2) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "expected `%s' to match `%s'",
            BlimpSymbol_GetName(sym1),
            BlimpSymbol_GetName(sym2)
        );
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus ExpectEQ(
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    (void)test;

    const BlimpSymbol *sym1;
    if (BlimpObject_ParseSymbol(args[0], &sym1) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    const BlimpSymbol *sym2;
    if (BlimpObject_ParseSymbol(args[1], &sym2) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    if (sym1 != sym2) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "expected `%s' to match `%s'",
            BlimpSymbol_GetName(sym1),
            BlimpSymbol_GetName(sym2)
        );
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus ExpectLT(
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    (void)test;

    const BlimpSymbol *sym1;
    if (BlimpObject_ParseSymbol(args[0], &sym1) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    size_t val1;
    if (!ParseUIntSymbol(sym1, &val1)) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "!expect_lt: argument 1 must be a numeric symbol (got `%s')",
            BlimpSymbol_GetName(sym1));
    }

    const BlimpSymbol *sym2;
    if (BlimpObject_ParseSymbol(args[1], &sym2) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    size_t val2;
    if (!ParseUIntSymbol(sym2, &val2)) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "!expect_lt: argument 2 must be a numeric symbol (got `%s')",
            BlimpSymbol_GetName(sym2));
    }

    if (val1 >= val2) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "expected `%zu' to be less than `%zu'", val1, val2);
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus ExpectPercent(
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    (void)test;

    const BlimpSymbol *percent_sym;
    if (BlimpObject_ParseSymbol(args[0], &percent_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    size_t percent;
    if (!ParseUIntSymbol(percent_sym, &percent)) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "!expect_percent: argument 1 must be a numeric symbol (got `%s')",
            BlimpSymbol_GetName(percent_sym));
    }

    const BlimpSymbol *actual_sym;
    if (BlimpObject_ParseSymbol(args[1], &actual_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    size_t actual;
    if (!ParseUIntSymbol(actual_sym, &actual)) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "!expect_percent: argument 2 must be a numeric symbol (got `%s')",
            BlimpSymbol_GetName(actual_sym));
    }

    const BlimpSymbol *expected_sym;
    if (BlimpObject_ParseSymbol(args[2], &expected_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    size_t expected;
    if (!ParseUIntSymbol(expected_sym, &expected)) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "!expect_percent: argument 3 must be a numeric symbol (got `%s')",
            BlimpSymbol_GetName(expected_sym));
    }

    if (((float)actual - (float)expected)/expected > (float)percent/100) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "expected `%zu' to match `%zu' within %zu",
            actual, expected, percent
        );
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus ExpectError(
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    (void)test;

    // Send the object a message (any message will do) to evaluate its body.
    if (Blimp_Send(blimp, args[0], args[0], args[0], result) == BLIMP_OK) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR, "expected error");
    }

    return VoidReturn(blimp, result);
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
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    const Options *options = &test->options;

    size_t iter;
    size_t ops;
    size_t warmup;

    // The first argument is a block which evaluates to the name of the
    // benchmark. When evaluated, it also (optionally) sets some symbols to
    // control various parameters (n, ops, warmup).
    BlimpObject *params = args[0];
    const BlimpSymbol *name;
    if (Blimp_SendAndParseSymbol(
            blimp, params, params, params, &name) != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    // Get the number of iterations to run.
    const BlimpSymbol *iter_key;
    if (Blimp_GetSymbol(blimp, "n", &iter_key) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    BlimpObject *iter_obj;
    const BlimpSymbol *iter_sym;
    if (BlimpObject_Get(params, iter_key, &iter_obj) != BLIMP_OK ||
        BlimpObject_ParseSymbol(iter_obj, &iter_sym) != BLIMP_OK ||
        !ParseUIntSymbol(iter_sym, &iter))
    {
        iter = 1000;
            // Do 1000 iterations by default.
    }

    // Clean up our reference to `n`.
    BlimpObject_Set(params, iter_key, params);

    // Get the number of operations per iteration.
    const BlimpSymbol *ops_key;
    if (Blimp_GetSymbol(blimp, "ops", &ops_key) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    BlimpObject *ops_obj;
    const BlimpSymbol *ops_sym;
    if (BlimpObject_Get(params, ops_key, &ops_obj) != BLIMP_OK ||
        BlimpObject_ParseSymbol(ops_obj, &ops_sym) != BLIMP_OK ||
        !ParseUIntSymbol(ops_sym, &ops))
    {
        ops = 1;
            // By default, each iteration is considered one operation.
    }

    // Clean up our reference to `ops`.
    BlimpObject_Set(params, ops_key, params);

    // Get the number of warmup iterations.
    const BlimpSymbol *warmup_key;
    if (Blimp_GetSymbol(blimp, "warmup", &warmup_key) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    BlimpObject *warmup_obj;
    const BlimpSymbol *warmup_sym;
    if (BlimpObject_Get(params, warmup_key, &warmup_obj) != BLIMP_OK ||
        BlimpObject_ParseSymbol(warmup_obj, &warmup_sym) != BLIMP_OK ||
        !ParseUIntSymbol(warmup_sym, &warmup))
    {
        warmup = 0;
            // No warmup by default.
    }

    // Clean up our reference to `warmup`.
    BlimpObject_Set(params, warmup_key, params);

    // Do warmup
    for (size_t i = 0; i < warmup; ++i) {
        // Send a message (any message will do) to the benchmark block to
        // evaluate its body.
        if (Blimp_Send(blimp, args[1], args[1], args[1], result) != BLIMP_OK) {
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

        if (Blimp_Send(blimp, args[1], args[1], args[1], result) != BLIMP_OK) {
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
        printf("  Total time: %.3fs\n", (float)total/cycles_per_ns/1000000000);
        printf("  Time/Op:    %.3f%s +- %.3f%s\n", avg_time, avg_unit, std_dev, std_dev_unit);
        printf("  Min:        %.3f%s\n", min_time, min_unit);
        printf("  Max:        %.3f%s\n", max_time, max_unit);
    }


    return VoidReturn(blimp, result);
}

void PrintPerfReportHeader(FILE *perf_report)
{
    fprintf(perf_report, "group,test,benchmark,iterations,ops/iteration,cycles/ns,avg,min,max\n");
}

static BlimpStatus GC_Unreachable(
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    (void)test;
    (void)args;

    BlimpGCStatistics stats = Blimp_GetGCStatistics(blimp);
    size_t unreachable = stats.allocated - stats.reachable;

    const BlimpSymbol *sym;
    if (MakeUIntSymbol(blimp, unreachable, &sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    return BlimpObject_NewSymbol(blimp, sym, result);
}

static BlimpStatus GC_ExpectClean(
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    (void)test;
    (void)args;

    BlimpGCStatistics stats = Blimp_GetGCStatistics(blimp);

    // All allocated objects should be reachable.
    if (stats.reachable != stats.allocated) {
        if (test->options.verbosity >= VERB_DEBUG) {
            // At the highest verbosity level, print a description of all the
            // unreachable objects and the references between them.
            Blimp_DumpUnreachable(stderr, blimp);
        }

        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "expected a clean heap, but found %zu unreachable objects",
            stats.allocated - stats.reachable);
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus GC_CheckCollect(
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    (void)test;
    (void)args;

    Blimp_CollectGarbage(blimp);
    BlimpGCStatistics stats = Blimp_GetGCStatistics(blimp);

    // All allocated objects should be reachable.
    if (stats.reachable != stats.allocated) {
        if (test->options.verbosity >= VERB_DEBUG) {
            // At the highest verbosity level, print a description of all the
            // unreachable objects and the references between them.
            Blimp_DumpUnreachable(stderr, blimp);
        }

        return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
            "expected a clean heap, but found %zu unreachable objects",
            stats.allocated - stats.reachable);
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus GC_Collect(
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    (void)test;
    (void)args;

    Blimp_CollectGarbage(blimp);
    return VoidReturn(blimp, result);
}

static BlimpStatus GC_PrintStats(
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    (void)args;

    const Options *options = &test->options;

    if (options->verbosity < VERB_STATS) {
        return VoidReturn(blimp, result);
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

    return VoidReturn(blimp, result);
}

static BlimpStatus PrintCode(
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    (void)args;

    const Options *options = &test->options;

    if (options->verbosity >= VERB_DEBUG) {
        BlimpBytecode_Print(stdout, Blimp_GlobalBytecode(blimp), true);
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus Eval(
    Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result)
{
    (void)test;

    const BlimpSymbol *code;
    if (BlimpObject_ParseSymbol(args[0], &code) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    BlimpParseTree tree;
    if (Blimp_ParseString(blimp, BlimpSymbol_GetName(code), &tree) != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    BlimpExpr *expr;
    if (BlimpParseTree_Eval(blimp, &tree, &expr) != BLIMP_OK) {
        BlimpParseTree_Destroy(&tree);
        return Blimp_Reraise(blimp);
    }
    BlimpParseTree_Destroy(&tree);

    BlimpStatus status = Blimp_Eval(
        blimp, expr, Blimp_CurrentScope(blimp), result);
    Blimp_FreeExpr(expr);
    return status;
}

////////////////////////////////////////////////////////////////////////////////
// The TestBlimp object (bound to ! in the global scope).
//
// The TestBlimp is an object which receives method names (which must be
// symbols) and returns TestBlimpMethodCall objects, which collect method
// arguments one at a time in curried fashion until there are enough arguments
// to call the appropriate method handler (one of the functions defined above).
//

typedef struct {
    const char *name;
    size_t nargs;
    BlimpStatus(*run)(
        Blimp *blimp, Test *test, BlimpObject **args, BlimpObject **result);
} TestBlimpMethod;

typedef struct {
    Test *test;
    const TestBlimpMethod *method;
    size_t nargs;
    BlimpObject *args[MAX_ARGUMENTS];
} TestBlimpMethodCall;

static BlimpStatus TestBlimpMethodCall_Receive(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)scope;

    TestBlimpMethodCall *call;
    if (BlimpObject_ParseExtension(
            receiver, NULL, (void **)&call) != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    assert(call->nargs < call->method->nargs);
    assert(call->nargs < MAX_ARGUMENTS);

    // Save the message as an argument, which we will later pass to the
    // method handler.
    BlimpObject_Borrow(message);
    call->args[call->nargs++] = message;

    if (call->nargs >= call->method->nargs) {
        // If we have enough arguments to call the method, do that and return
        // the result.
        BlimpStatus status = call->method->run(
            blimp, call->test, call->args, result);

        // Once we have called the method, we can release our references to its
        // arguments.
        while (call->nargs > 0) {
            BlimpObject_Release(call->args[--call->nargs]);
        }

        return status;
    } else {
        // Return this object so we can collect the next argument.
        BlimpObject_Borrow(receiver);
        *result = receiver;
        return BLIMP_OK;
    }
}

static BlimpStatus TestBlimpMethodCall_New(
    Blimp *blimp,
    Test *test,
    const TestBlimpMethod *method,
    BlimpObject **result)
{
    if (method->nargs == 0) {
        // Just call the method immediately and return the result.
        return method->run(blimp, test, NULL, result);
    }

    // Create an object which will collect arguments as we receive them in
    // curried fashion.
    TestBlimpMethodCall *call = malloc(sizeof(TestBlimpMethodCall));
    if (call == NULL) {
        return Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY);
    }
    call->method = method;
    call->test   = test;
    call->nargs  = 0;

    // Create a new extension object which will consume arguments until it has
    // enough, and the call the method.
    return BlimpObject_NewExtension(
        blimp,
        Blimp_GlobalObject(blimp),
        call,
        TestBlimpMethodCall_Receive,
        free,
        result);
}

static const TestBlimpMethod methods[] = {
    {"expect",              1,  Expect          },
    {"expect_eq",           2,  ExpectEQ        },
    {"expect_lt",           2,  ExpectLT        },
    {"expect_percent",      3,  ExpectPercent   },
    {"expect_error",        1,  ExpectError     },
    {"benchmark",           2,  Benchmark       },
    {"gc_unreachable",      0,  GC_Unreachable  },
    {"gc_collect",          0,  GC_Collect      },
    {"gc_expect_clean",     0,  GC_ExpectClean  },
    {"gc_check_collect",    0,  GC_CheckCollect },
    {"gc_print_stats",      0,  GC_PrintStats   },
    {"print_code",          0,  PrintCode       },
    {"eval",                1,  Eval            },
};

static BlimpStatus TestBlimp_Receive(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)scope;

    Test *test;
    if (BlimpObject_ParseExtension(receiver, NULL, (void **)&test)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    // The message is supposed to be the name of one of the methods defined in
    // the table above.
    const BlimpSymbol *method_name;
    if (BlimpObject_ParseSymbol(message, &method_name) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    // Search for a method with a matching name.
    for (size_t i = 0; i < sizeof(methods)/sizeof(methods[0]); ++i) {
        if (strcmp(BlimpSymbol_GetName(method_name), methods[i].name) == 0) {
            return TestBlimpMethodCall_New(blimp, test, &methods[i], result);
        }
    }

    return Blimp_ErrorMsg(blimp, BLIMP_ERROR, "no such method :%s",
        BlimpSymbol_GetName(method_name));
}

Blimp *TestBlimp_New(Test *test)
{
    Blimp *blimp = Blimp_New(&test->options.blimp_options);
    if (blimp == NULL) {
        return NULL;
    }

    // Create the TestBlimp.
    BlimpObject *obj;
    if (BlimpObject_NewExtension(
            blimp,
            Blimp_GlobalObject(blimp),
            test,
            TestBlimp_Receive,
            NULL,
            &obj) != BLIMP_OK)
    {
        Blimp_Delete(blimp);
        return NULL;
    }

    // Bind the TestBlimp to the global symbol `:'.
    const BlimpSymbol *colon;
    if (Blimp_GetSymbol(blimp, ":", &colon) != BLIMP_OK) {
        Blimp_Delete(blimp);
        return NULL;
    }
    if (BlimpObject_Set(Blimp_GlobalObject(blimp), colon, obj) != BLIMP_OK) {
        Blimp_Delete(blimp);
        return NULL;
    }
    BlimpObject_Release(obj);

    // Initialize REPL commands (`?').
    if (InitCommands(blimp) != BLIMP_OK) {
        Blimp_Delete(blimp);
        return NULL;
    }

    return blimp;
}
