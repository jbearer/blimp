#ifndef BLIMP_SYSTEM_H
#define BLIMP_SYSTEM_H

#include <blimp/module.h>

#include "hash_map.h"

BlimpStatus Function(
    Blimp *blimp, const char *name, BlimpMethod method, void *arg);
BlimpStatus VoidReturn(Blimp *blimp, BlimpObject **result);

////////////////////////////////////////////////////////////////////////////////
// Extension Classes
//
// The Class interface can be used to create class with virtual dispatch that
// mimics prelude classes, but whose methods are implemented in C.
//
// To declare a new extension class:
//
//  Class my_class;
//  Class_Init(blimp, &my_class, "my_class",
//      (Constructor){nargs, ConstructorFunction},
//      (Method[]) {
//          {"method1", nargs, Method1Function},
//          {"method2", nargs, Method2Function},
//          {0}
//      }
//  );
//
// The final `{0}` method is important; it indicates the end of the variably
// sized array of methods.
//
// After a call to Class() succeeds, bl:mp programs will be able to create
// instances of the extension class using the syntax `my_class arg1 arg2 ...`.
// This will cause arguments `arg1`, `arg2`, ... to be passed to the C function
// `ConstructorFunction`. The constructor returns a pointer-sized piece of state
// which will be passed to method functions whenever a method is invoked on the
// resulting instance.
//
// Extension methods are invoked by sending a message to the result of invoking
// and extension constructor. For example,
//      (my_class arg1 arg2 ...) method1 margs
// will call the C function Method1Function with the argument `margs` and
// whatever state was returned by ConstructorFunction.
//
// Instances of extension classes can also be created in C code using
// Instance(), and the internal state of an instance can be extracted from a
// bl:mp extension object using ParseInstance().

typedef struct {
    size_t num_args;
    BlimpStatus(*call)(Blimp *blimp, BlimpObject **args, void **state);
} Constructor;

typedef struct {
    const char *name;
    size_t num_args;
    BlimpStatus(*call)(
        Blimp *blimp, BlimpObject **args, void *state, BlimpObject **result);
} Method;

typedef struct {
    const BlimpSymbol *name;
    Constructor constr;
    Method constr_method;
    HashMap vtable;
} Class;

BlimpStatus Class_Init(
    Blimp *blimp,
    Class *cls,
    const char *name,
    const Constructor *constructor,
    const Method *methods);
BlimpStatus Instance(
    Blimp *blimp, const Class *cls, void *state, BlimpObject **instance);
BlimpStatus ParseInstance(
    Blimp *blimp, const Class *cls, BlimpObject *obj, void **state);

#endif
