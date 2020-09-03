#include <assert.h>
#include <string.h>

#include <blimp.h>
#include "command.h"

static inline BlimpStatus VoidReturn(Blimp *blimp, BlimpObject **result)
{
    BlimpObject_Borrow(Blimp_GlobalObject(blimp));
    *result = Blimp_GlobalObject(blimp);
    return BLIMP_OK;
}

////////////////////////////////////////////////////////////////////////////////
// Command Runner
//
// The command runner consists of two kinds of extension objects:
//  * A CommandServer, which holds a list of command descriptions, waits to
//    receive a symbol matching the name of a recognized command, and then
//    dispatches a CommandCall to collect the arguments for that command.
//  * A CommandCall for a specific command, which collects argument objects
//    until it has enough to call the command's handler function.
//
// A singleton command server will be bound to the global object ?, so that the
// syntax `?command [args...]' can be used to access top-level commands. Any
// command may also return a command server of its own, in order to implement
// sub-commands.
//

#define MAX_ARGUMENTS 4

typedef struct {
    const char *name;
    const char *help;
    BlimpStatus(*run)(Blimp *blimp, BlimpObject **args, BlimpObject **result);
    size_t nargs;
} Command;

typedef struct {
    Blimp *blimp;
    const Command *command;
    BlimpObject *args[MAX_ARGUMENTS];
    size_t nargs;
} CommandCall;

static BlimpStatus CommandCall_Receive(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)scope;

    CommandCall *call;
    if (BlimpObject_ParseExtension(
            receiver, NULL, (void **)&call) != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    assert(call->nargs < call->command->nargs);
    assert(call->nargs < MAX_ARGUMENTS);

    // Save the message as an argument, which we will later pass to the
    // command handler.
    BlimpObject_Borrow(message);
    call->args[call->nargs++] = message;

    if (call->nargs >= call->command->nargs) {
        // If we have enough arguments to call the command, do that and return
        // the result.
        BlimpStatus status = call->command->run(blimp, call->args, result);

        // Once we have called the command, we can release our references to its
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

static BlimpStatus CommandServer_Receive(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)context;

    const Command *commands;
    if (BlimpObject_ParseExtension(
            receiver, NULL, (void **)&commands) != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    const BlimpSymbol *command;
    if (BlimpObject_ParseSymbol(message, &command) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }
    const char *name = BlimpSymbol_GetName(command);

    // All CommandServers recognize the special command `help', which just
    // prints the name and help string of all the commands recognized by that
    // server.
    if (strcmp(name, "help") == 0) {
        printf("Commands:\n");
        for (const Command *command = commands; command->name; ++command) {
            printf("  %s - %s\n", command->name, command->help);
        }
        return VoidReturn(blimp, result);
    }

    // Search for a command whose name matches the given command.
    for (const Command *command = commands; command->name; ++command) {
        if (strcmp(name, command->name) == 0) {
            if (command->nargs == 0) {
                // If the command does not require arguments, invoke the handler
                // directly.
                return command->run(blimp, NULL, result);
            }

            // Otherwise, create a CommandCall to collect the proper number of
            // arguments and then call the handler.
            CommandCall *call = malloc(sizeof(CommandCall));
            if (call == NULL) {
                return Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY);
            }

            call->blimp = blimp;
            call->command = command;
            call->nargs = 0;
            return BlimpObject_NewExtension(
                blimp,
                Blimp_GlobalObject(blimp),
                call,
                CommandCall_Receive,
                free,
                result);
        }
    }

    return Blimp_ErrorMsg(blimp, BLIMP_ERROR,
        "unknown command `%s'. Try ?help.", name);
}

// Create a CommandServer which recognizes a given list of commands.
//
// `commands` should be an array of Command data structures, terminated by a
// Command which is all zero.
static BlimpStatus CommandServer_New(
    Blimp *blimp, const Command *commands, BlimpObject **server)
{
    return BlimpObject_NewExtension(
        blimp,
        Blimp_GlobalObject(blimp),
        (void *)commands,
        CommandServer_Receive,
        NULL,
        server);
}

////////////////////////////////////////////////////////////////////////////////
// ?inspect
//

// Given a symbol object representing a numerical memory address, return the
// Object at that address. The address is trusted, no memory checking is
// performed.
static BlimpStatus ParseAddress(
    Blimp *blimp, BlimpObject *addr, BlimpObject **obj)
{
    const BlimpSymbol *address_sym;
    if (BlimpObject_ParseSymbol(addr, &address_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    char *address_end;
    *obj = (BlimpObject *)strtol(
        BlimpSymbol_GetName(address_sym), &address_end, 0);
    if (*address_end) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR, "expecting an address");
    }

    return BLIMP_OK;;
}

static BlimpStatus InspectUnreachable(
    Blimp *blimp, BlimpObject **args, BlimpObject **result)
{
    (void)args;
    Blimp_DumpUnreachable(stdout, blimp);
    return VoidReturn(blimp, result);
}

static void PrintObjectChild(
    Blimp *blimp,
    BlimpObject *obj,
    const BlimpSymbol *child_name,
    BlimpObject *child,
    void *arg)
{
    (void)blimp;
    (void)obj;
    (void)arg;

    printf("  %-10s -> %p\n", BlimpSymbol_GetName(child_name), child);
}

static BlimpStatus InspectObject(
    Blimp *blimp, BlimpObject **args, BlimpObject **result)
{
    BlimpObject *obj = NULL;
    if (ParseAddress(blimp, args[0], &obj) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    BlimpObjectInfo info;
    BlimpObject_Inspect(obj, &info);

    BlimpObject_Print(stdout, obj);
    printf("\n\n");

    printf("GC State\n");
    printf("  refcount: %zu\n",       info.refcount);
    printf("  clump: %p\n",           info.clump);
    printf("  clump_refcount: %zu\n", info.clump_refcount);
    printf("Children\n");
    BlimpObject_ForEachChild(obj, PrintObjectChild, NULL);

    return VoidReturn(blimp, result);
}

typedef struct {
    BlimpObject *target;
    bool matched;
} ObjectMatch;

static void PrintMatchingChildren(
    Blimp *blimp,
    BlimpObject *obj,
    const BlimpSymbol *child_name,
    BlimpObject *child,
    void *arg)
{
    (void)blimp;

    ObjectMatch *match = (ObjectMatch *)arg;

    if (child == match->target) {
        if (!match->matched) {
            printf("%p:\n", obj);
            match->matched = true;
        }

        printf("  %-10s -> %p\n", BlimpSymbol_GetName(child_name), child);
    }
}

static void PrintOwningObjects(Blimp *blimp, BlimpObject *obj, void *arg)
{
    (void)blimp;

    ObjectMatch match = { (BlimpObject *)arg, false };
    BlimpObject_ForEachChild(obj, PrintMatchingChildren, &match);
    if (match.matched) {
        printf("\n");
    }
}

static BlimpStatus InspectOwners(
    Blimp *blimp, BlimpObject **args, BlimpObject **result)
{
    BlimpObject *obj = NULL;
    if (ParseAddress(blimp, args[0], &obj) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    Blimp_ForEachObject(blimp, PrintOwningObjects, obj);
    return VoidReturn(blimp, result);
}

static void PrintChildrenInClump(
    Blimp *blimp,
    BlimpObject *obj,
    const BlimpSymbol *child_name,
    BlimpObject *child,
    void *arg)
{
    (void)blimp;

    ObjectMatch *match = (ObjectMatch *)arg;
    BlimpObjectInfo child_info;
    BlimpObject_Inspect(child, &child_info);

    if (child_info.clump == match->target) {
        if (!match->matched) {
            printf("%p:\n", obj);
            match->matched = true;
        }

        printf("  %-10s -> %p\n", BlimpSymbol_GetName(child_name), child);
    }
}

static void PrintClumpMember(Blimp *blimp, BlimpObject *obj, void *arg)
{
    (void)blimp;

    BlimpObject *clump = (BlimpObject *)arg;
    BlimpObjectInfo info;
    BlimpObject_Inspect(obj, &info);

    if (info.clump == clump) {
        printf("\n%p: ", obj);
        BlimpObject_Print(stdout, obj);
        printf("\n");

        ObjectMatch match = {clump, true};
        BlimpObject_ForEachChild(obj, PrintChildrenInClump, &match);
    }
}

static BlimpStatus InspectClump(
    Blimp *blimp, BlimpObject **args, BlimpObject **result)
{
    BlimpObject *obj = NULL;
    if (ParseAddress(blimp, args[0], &obj) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    BlimpObjectInfo info;
    BlimpObject_Inspect(obj, &info);
    BlimpObject *clump = info.clump ? info.clump : obj;

    printf("Representative:  %p\n", clump);
    printf("Reference count: %zu\n", info.clump_refcount);

    Blimp_ForEachObject(blimp, PrintClumpMember, clump);

    return VoidReturn(blimp, result);
}

static void PrintOwnersOfClump(Blimp *blimp, BlimpObject *obj, void *arg)
{
    (void)blimp;

    BlimpObject *clump = (BlimpObject *)arg;
    BlimpObjectInfo info;
    BlimpObject_Inspect(obj, &info);
    if (info.clump == clump) {
        return;
    }

    ObjectMatch match = { clump, false };
    BlimpObject_ForEachChild(obj, PrintChildrenInClump, &match);
    if (match.matched) {
        printf("\n");
    }
}

static BlimpStatus InspectClumpOwners(
    Blimp *blimp, BlimpObject **args, BlimpObject **result)
{
    BlimpObject *obj = NULL;
    if (ParseAddress(blimp, args[0], &obj) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    BlimpObjectInfo info;
    BlimpObject_Inspect(obj, &info);
    BlimpObject *clump = info.clump ? info.clump : obj;
    Blimp_ForEachObject(blimp, PrintOwnersOfClump, clump);
    return VoidReturn(blimp, result);
}

static const Command inspect_commands[] = {
    {"unreachable", "print information about unreachable heap objects",
        InspectUnreachable, 0},
    {"object", "print information about the internal state of an object",
        InspectObject, 1},
    {"owners", "print the owners of an object",
        InspectOwners, 1},
    {"clump", "print the clump containing an object",
        InspectClump, 1},
    {"clump_owners", "print the owners of a clump",
        InspectClumpOwners, 1},
    {0}
};

static BlimpStatus Inspect(
    Blimp *blimp, BlimpObject **args, BlimpObject **result)
{
    (void)args;
    return CommandServer_New(blimp, inspect_commands, result);
}


////////////////////////////////////////////////////////////////////////////////
// Top-level commands
//

static const Command commands[] = {
    {"inspect", "inspect interpreter state", Inspect, 0},
    {0}
};

BlimpStatus InitCommands(Blimp *blimp)
{
    const BlimpSymbol *command_symbol;
    if (Blimp_GetSymbol(blimp, "?", &command_symbol) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    BlimpObject *server;
    if (CommandServer_New(blimp, commands, &server) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    if (BlimpObject_Set(Blimp_GlobalObject(blimp), command_symbol, server)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    BlimpObject_Release(server);
    return BLIMP_OK;
}
