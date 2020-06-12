#include <blimp/module.h>

static inline void VoidReturn(Blimp *blimp, BlimpObject **result)
{
    BlimpObject_Borrow(Blimp_GlobalObject(blimp));
    *result = Blimp_GlobalObject(blimp);
}

static BlimpStatus Print(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    void *data,
    BlimpObject **result)
{
    (void)scope;
    (void)receiver;
    (void)data;

    BlimpObject_Print(stdout, message);
    fputc('\n', stdout);

    VoidReturn(blimp, result);
    return BLIMP_OK;
}

static BlimpStatus InitSystemIO(
    Blimp *blimp, BlimpObject *context, BlimpObject **result)
{
    (void)context;

    VoidReturn(blimp, result);

    return Blimp_BindVTableFragment(blimp, (BlimpVTableFragment){
        {"print", "_", Print, NULL},
        {0, 0, 0, 0}
    });
}

BLIMP_MODULE(InitSystemIO);
