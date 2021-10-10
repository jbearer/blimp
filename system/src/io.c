#include "system.h"

////////////////////////////////////////////////////////////////////////////////
// Output streams
//
// An output stream is an object which receives symbols and writes them...
// somewhere. These streams write to the C stdio files. There is a global output
// stream named `stdout` and another named `stderr`.
//

static BlimpStatus OutStream(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)scope;

    FILE *file;
    BlimpMethod method;
    if (BlimpObject_ParseExtension(receiver, &method, (void **)&file)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }
    if (method != OutStream) {
        return Blimp_ErrorMsg(
            blimp, BLIMP_ERROR, "expected system.io.OutStream");
    }

    const BlimpSymbol *sym;
    if (BlimpObject_ParseSymbol(message, &sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    fprintf(file, "%s", BlimpSymbol_GetName(sym));

    // Return the receiver so that multiple outputs can be sent to the stream in
    // a chain.
    *result = BlimpObject_Borrow(receiver);
    return BLIMP_OK;
}

////////////////////////////////////////////////////////////////////////////////
// Output functions
//

static BlimpStatus Dump(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)scope;
    (void)receiver;

    BlimpObject_Print(stdout, message);
    putchar('\n');

    VoidReturn(blimp, result);
    return BLIMP_OK;
}

static BlimpStatus Print(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    // `print obj` is equivalent to `obj render stdout`.

    (void)receiver;

    // Create an output stream.
    BlimpObject *out;
    if (BlimpObject_NewExtension(blimp, scope, stdout, OutStream, NULL, &out)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    // Send the message `render` to the object, returning its implementation of
    // the `render` method.
    const BlimpSymbol *render;
    if (Blimp_GetSymbol(blimp, "render", &render) != BLIMP_OK) {
        BlimpObject_Release(out);
        return Blimp_Reraise(blimp);
    }
    BlimpObject *render_msg;
    if (BlimpObject_NewSymbol(blimp, render, &render_msg) != BLIMP_OK) {
        BlimpObject_Release(out);
        return Blimp_Reraise(blimp);
    }
    BlimpObject *render_handler;
    if (Blimp_Send(
            blimp, scope, message, render_msg, &render_handler) != BLIMP_OK)
    {
        BlimpObject_Release(out);
        BlimpObject_Release(render_msg);
        return Blimp_Reraise(blimp);
    }
    BlimpObject_Release(render_msg);

    // Send the output stream to the render handler.
    if (Blimp_Send(blimp, scope, render_handler, out, NULL) != BLIMP_OK) {
        BlimpObject_Release(out);
        BlimpObject_Release(render_handler);
        return Blimp_Reraise(blimp);
    }

    BlimpObject_Release(out);
    BlimpObject_Release(render_handler);

    putchar('\n');
    return VoidReturn(blimp, result);
}

static BlimpStatus InitSystemIO(
    Blimp *blimp, BlimpObject *context, BlimpObject **result)
{
    (void)context;

    VoidReturn(blimp, result);

    if (Function(blimp, "stdout", OutStream, stdout) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    if (Function(blimp, "stderr", OutStream, stderr) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    if (Function(blimp, "dump", Dump, NULL) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    if (Function(blimp, "print", Print, NULL) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return BLIMP_OK;
}

BLIMP_MODULE(InitSystemIO);
