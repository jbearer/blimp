#include "options.h"
#include "test_blimp.h"

static bool ParseUIntSymbol(const BlimpSymbol *sym, size_t *result)
{
    char *invalid_char;
    *result = strtol(BlimpSymbol_GetName(sym), &invalid_char, 0);
    return !*invalid_char;
}

static BlimpStatus MakeUIntSymbol(
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

    BlimpObject_Borrow(receiver);
    *result = receiver;
    return BLIMP_OK;
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
    (void)result;

    return Blimp_ErrorMsg(
        blimp, BLIMP_ERROR, "garbage collection is not yet implemented");
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
    (void)message;

    const Options *options = data;

    BlimpObject_Borrow(receiver);
    *result = receiver;

    if (options->verbosity < VERB_STATS) {
        return BLIMP_OK;
    }

    BlimpGCStatistics stats = Blimp_GetGCStatistics(blimp);
    printf("GC Statistics:\n");
    printf("  allocated now:   %zu\n", stats.allocated);
    printf("  reachable:       %zu\n", stats.reachable);
    printf("  high water mark: %zu\n", stats.max_allocated);
    return BLIMP_OK;
}

Blimp *TestBlimp_New(const Options *options)
{
    Blimp *blimp = Blimp_New(NULL);
    if (blimp == NULL) {
        return blimp;
    }

    BlimpVTableFragment table = {
        { "symbol", "!expect",          Expect,           NULL },
        { "symbol", "!expect_percent",  ExpectPercent,    NULL },
        { "_",      "!expect_error",    ExpectError,      NULL },
        { "gc",     "!allocated",       GC_Allocated,     NULL },
        { "gc",     "!reachable",       GC_Reachable,     NULL },
        { "gc",     "!high_water_mark", GC_HighWaterMark, NULL },
        { "gc",     "!collect",         GC_Collect,       NULL },
        { "gc",     "!print_stats",     GC_PrintStats,    (void *)options },
        { 0, 0, 0, 0}
    };
    if (Blimp_BindVTableFragment(blimp, table) != BLIMP_OK) {
        Blimp_Delete(blimp);
        return NULL;
    }

    return blimp;
}
