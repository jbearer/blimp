#include "test_blimp.h"

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

Blimp *TestBlimp_New(const BlimpOptions *options)
{
    Blimp *blimp = Blimp_New(options);
    if (blimp == NULL) {
        return blimp;
    }

    BlimpVTableFragment table = {
        { "symbol", "!expect",       Expect,      NULL },
        { "_",      "!expect_error", ExpectError, NULL },
        { 0, 0, 0, 0}
    };
    if (Blimp_BindVTableFragment(blimp, table) != BLIMP_OK) {
        Blimp_Delete(blimp);
        return NULL;
    }

    return blimp;
}
