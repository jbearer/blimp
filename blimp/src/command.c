#include <assert.h>
#include <string.h>

#include <blimp.h>
#include "command.h"
#include "debug.h"

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
    BlimpStatus(*run)(
        Blimp *blimp,
        BlimpObject *scope,
        BlimpObject **args,
        void *arg,
        BlimpObject **result);
    size_t nargs;
    void *arg;
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
        BlimpStatus status = call->command->run(
            blimp, scope, call->args, call->command->arg, result);

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
                return command->run(blimp, context, NULL, command->arg, result);
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
    // Figure out how many commands there are.
    size_t num_commands = 0;
    for (const Command *command = commands; command->name; ++command) {
        ++num_commands;
    }

    // Allocate our own copy of `commands`, so we can expand it later if needed.
    // Allocate space for a terminating null Command.
    Command *commands_copy = malloc((num_commands + 1)*sizeof(Command));
    if (commands_copy == NULL) {
        return Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY);
    }
    memcpy(commands_copy, commands, (num_commands + 1)*sizeof(Command));

    return BlimpObject_NewExtension(
        blimp,
        Blimp_GlobalObject(blimp),
        (void *)commands_copy,
        CommandServer_Receive,
        free,
        server);
}

static BlimpStatus CommandServer_AddCommands(
    Blimp *blimp, BlimpObject *server, const Command *new_commands)
{
    const Command *old_commands;
    if (BlimpObject_ParseExtension(server, NULL, (void **)&old_commands)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    // Figure out the total number of commands.
    size_t num_old_commands = 0;
    for (const Command *command = old_commands; command->name; ++command) {
        ++num_old_commands;
    }
    size_t num_new_commands = 0;
    for (const Command *command = new_commands; command->name; ++command) {
        ++num_new_commands;
    }

    // Allocate space for the new commands, plus a terminating null Command.
    Command *commands = realloc(
        (void *)old_commands,
        (num_old_commands+num_new_commands+1)*sizeof(Command)
    );
    if (commands == NULL) {
        return Blimp_Error(blimp, BLIMP_OUT_OF_MEMORY);
    }
    memcpy(
        commands + num_old_commands,
        new_commands,
        (num_new_commands+1)*sizeof(Command)
    );

    return BlimpObject_SetExtensionState(server, commands);
}

// Given a symbol object representing a numerical memory address, return an
// untyped pointer to that address. The address is trusted, no memory checking
// is performed.
static BlimpStatus ParseAddress(Blimp *blimp, BlimpObject *addr, void **p)
{
    const BlimpSymbol *address_sym;
    if (BlimpObject_ParseSymbol(addr, &address_sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    char *address_end;
    *p = (void *)strtol(
        BlimpSymbol_GetName(address_sym), &address_end, 0);
    if (*address_end) {
        return Blimp_ErrorMsg(blimp, BLIMP_ERROR, "expecting an address");
    }

    return BLIMP_OK;
}

// Given a symbol object representing a numerical memory address, return the
// Object at that address. The address is trusted, no memory checking is
// performed.
static BlimpStatus ParseObjectAddress(
    Blimp *blimp, BlimpObject *addr, BlimpObject **obj)
{
    return ParseAddress(blimp, addr, (void **)obj);
}

////////////////////////////////////////////////////////////////////////////////
// ?inspect
//

static BlimpStatus InspectUnreachable(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)args;
    (void)arg;

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

static void PrintObjectInfo(BlimpObject *obj)
{
    BlimpObjectInfo info;
    BlimpObject_Inspect(obj, &info);

    BlimpObject_Print(stdout, obj);
    printf("\n\n");

    BlimpBytecode *code;
    if (BlimpObject_ParseBlock(obj, &code) == BLIMP_OK) {
        printf("Code\n");
        BlimpBytecode_Print(stdout, code, true);
    }

    printf("GC State\n");
    printf("  refcount: %zu\n",       info.refcount);
    printf("  clump: %p\n",           info.clump);
    printf("  clump_refcount: %zu\n", info.clump_refcount);
    printf("Children\n");
    BlimpObject_ForEachChild(obj, PrintObjectChild, NULL);
}

static BlimpStatus InspectObject(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)arg;

    BlimpObject *obj = NULL;
    if (ParseObjectAddress(blimp, args[0], &obj) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    PrintObjectInfo(obj);

    *result = BlimpObject_Borrow(obj);
    return BLIMP_OK;
}

static BlimpStatus InspectExpr(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)blimp;
    (void)scope;
    (void)arg;

    PrintObjectInfo(args[0]);

    *result = BlimpObject_Borrow(args[0]);
    return BLIMP_OK;
}

static BlimpStatus PrintParseTree(
    Blimp *blimp, BlimpObject *context, BlimpObject *tree);

static Status ParseTreePrinter(
    Blimp *blimp,
    BlimpObject *context,
    BlimpObject *receiver,
    BlimpObject *message,
    BlimpObject **result)
{
    (void)receiver;

    TRY(PrintParseTree(blimp, context, message));
    printf(" ");

    *result = BlimpObject_Borrow(receiver);
    return BLIMP_OK;
}

static BlimpStatus PrintParseTree(
    Blimp *blimp, BlimpObject *context, BlimpObject *tree)
{
    BlimpObject *printer;
    TRY(BlimpObject_NewExtension(
        blimp, context, NULL, ParseTreePrinter, NULL, &printer));

    printf("(");
    const BlimpSymbol *symbol;
    if (Blimp_SendAndParseSymbol(blimp, context, tree, printer, &symbol)
            == BLIMP_OK)
    {
        printf("; %s", BlimpSymbol_GetName(symbol));
    }
    printf(")");

    BlimpObject_Release(printer);
    return BLIMP_OK;
}

static BlimpStatus InspectParseTree(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)arg;

    PrintParseTree(blimp, scope, args[0]);
    printf("\n");

    *result = BlimpObject_Borrow(args[0]);
    return BLIMP_OK;
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
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)arg;

    BlimpObject *obj = NULL;
    if (ParseObjectAddress(blimp, args[0], &obj) != BLIMP_OK) {
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
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)arg;

    BlimpObject *obj = NULL;
    if (ParseObjectAddress(blimp, args[0], &obj) != BLIMP_OK) {
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
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)arg;

    BlimpObject *obj = NULL;
    if (ParseObjectAddress(blimp, args[0], &obj) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    BlimpObjectInfo info;
    BlimpObject_Inspect(obj, &info);
    BlimpObject *clump = info.clump ? info.clump : obj;
    Blimp_ForEachObject(blimp, PrintOwnersOfClump, clump);
    return VoidReturn(blimp, result);
}

static BlimpStatus InspectBlimp(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)args;
    (void)arg;

    printf("Global object: %p\n", Blimp_GlobalObject(blimp));

    return VoidReturn(blimp, result);
}

static BlimpStatus InspectCode(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)arg;

    BlimpBytecode *code = NULL;
    if (ParseAddress(blimp, args[0], (void **)&code) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    BlimpBytecode_Print(stdout, code, false);
    return VoidReturn(blimp, result);
}

static BlimpStatus InspectSymbol(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)arg;

    const BlimpSymbol *sym;
    if (BlimpObject_ParseSymbol(args[0], &sym) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    BlimpObject *value;
    if (BlimpObject_Get(scope, sym, &value) != BLIMP_OK) {
        printf("`%s` is undefined\n", BlimpSymbol_GetName(sym));
        return VoidReturn(blimp, result);
    }

    printf("`%s` -> %p\n\n", BlimpSymbol_GetName(sym), value);
    PrintObjectInfo(value);

    return VoidReturn(blimp, result);
}

static BlimpStatus InspectGrammar(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)args;
    (void)arg;

    Blimp_DumpGrammarVitals(stdout, blimp);
    return VoidReturn(blimp, result);
}

static const Command inspect_commands[] = {
    {"unreachable", "print information about unreachable heap objects",
        InspectUnreachable, 0, NULL},
    {"object", "print information about the internal state of an object",
        InspectObject, 1, NULL},
    {"expr", "print information about the result of evaluating an expression",
        InspectExpr, 1, NULL},
    {"parse_tree", "pretty-print an object, interpreting it as a parse tree",
        InspectParseTree, 1, NULL},
    {"owners", "print the owners of an object",
        InspectOwners, 1, NULL},
    {"clump", "print the clump containing an object",
        InspectClump, 1, NULL},
    {"clump_owners", "print the owners of a clump",
        InspectClumpOwners, 1, NULL},
    {"blimp", "print information about the bl:mp interpreter",
        InspectBlimp, 0, NULL},
    {"code", "print a section of bytecode located at a given address",
        InspectCode, 1, NULL},
    {"symbol", "print information about the value of a symbol",
        InspectSymbol, 1, NULL},
    {"grammar", "print information about the current grammar",
        InspectGrammar, 0, NULL},
    {0}
};

static BlimpStatus Inspect(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)args;
    (void)scope;
    (void)arg;

    return CommandServer_New(blimp, inspect_commands, result);
}

////////////////////////////////////////////////////////////////////////////////
// ?db
//

static BlimpStatus DbAttach(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)args;

    Debugger *db = (Debugger *)arg;

    if (Debugger_Attach(db, blimp) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus DbDetach(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)args;

    Debugger *db = (Debugger *)arg;
    Debugger_Detach(db);
    return VoidReturn(blimp, result);
}

static BlimpStatus DbBreak(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;

    Debugger *db = (Debugger *)arg;

    const BlimpInstruction *bp = NULL;
    if (ParseAddress(blimp, args[0], (void **)&bp) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    if (Debugger_Break(db, bp, 1, NULL, false) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus DbTBreak(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;

    Debugger *db = (Debugger *)arg;

    const BlimpInstruction *bp = NULL;
    if (ParseAddress(blimp, args[0], (void **)&bp) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    if (Debugger_Break(db, bp, 1, NULL, true) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus DbStepI(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)args;

    Debugger *db = (Debugger *)arg;

    if (Debugger_StepI(db) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus DbNextI(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)args;

    Debugger *db = (Debugger *)arg;

    if (Debugger_NextI(db) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return VoidReturn(blimp, result);
}


static BlimpStatus DbFinish(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)args;

    Debugger *db = (Debugger *)arg;

    if (Debugger_Finish(db) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus DbContinue(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)args;

    Debugger *db = (Debugger *)arg;

    if (Debugger_Continue(db) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus DbBt(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)args;

    Debugger *db = (Debugger *)arg;

    if (Debugger_Bt(db) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus DbList(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)scope;
    (void)args;

    Debugger *db = (Debugger *)arg;

    if (Debugger_List(db) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    return VoidReturn(blimp, result);
}

static BlimpStatus Db(
    Blimp *blimp,
    BlimpObject *scope,
    BlimpObject **args,
    void *arg,
    BlimpObject **result)
{
    (void)args;
    (void)scope;
    (void)arg;

    Debugger *db = (Debugger *)arg;

    const Command commands[] = {
        {"attach", "attach to the current bl:mp process", DbAttach, 0, db},
        {"detach", "detach from the currently attached process", DbDetach, 0, db},
        {"break", "set a breakpoint", DbBreak, 1, db},
        {"tbreak", "set a temporary breakpoint", DbTBreak, 1, db},
        {"stepi", "step into the next instruction", DbStepI, 0, db},
        {"nexti", "step over the next instruction", DbNextI, 0, db},
        {"finish", "step out of the current procedure", DbFinish, 0, db},
        {"continue", "continue program execution", DbContinue, 0, db},
        {"bt", "print a stack trace", DbBt, 0, db},
        {"list", "print the current procedure", DbList, 0, db},
        {0},
    };

    return CommandServer_New(blimp, commands, result);
}

////////////////////////////////////////////////////////////////////////////////
// Top-level commands
//

BlimpStatus InitCommands(Blimp *blimp)
{
    static const Command commands[] = {
        {"inspect", "inspect interpreter state", Inspect, 0, NULL},
        {0}
    };

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

BlimpStatus InitDebuggerCommands(Blimp *blimp, Debugger *db)
{
    const Command commands[] = {
        {"db", "debugger commands", Db, 0, db},
        {0},
    };

    const BlimpSymbol *command_symbol;
    if (Blimp_GetSymbol(blimp, "?", &command_symbol) != BLIMP_OK) {
        return Blimp_Reraise(blimp);
    }

    BlimpObject *server;
    if (BlimpObject_Get(Blimp_GlobalObject(blimp), command_symbol, &server)
            != BLIMP_OK)
    {
        return Blimp_Reraise(blimp);
    }

    return CommandServer_AddCommands(blimp, server, commands);
}
