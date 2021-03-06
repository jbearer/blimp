////////////////////////////////////////////////////////////////////////////////
// bl:mp bytecode
//
// bl:mp bytecode is a low-level, assembly-like language with instructions
// representing the primitive operations supported by the bl:mp abstract macine.
// This is the language which is actually executed by the bl:mp interpreter;
// expressions are evaluated by internally compiling them to bytecode and then
// interpreting the bytecode. The key feature of bl:mp bytecode which makes it a
// better target for interpretation than expressions is that it is completely
// linear: tree-like expressions which contain nested subexpressions are
// serialized by the compiler into a sequence of bytecode instructions which
// first compute the subexpressions and then the outer expressoin; none of these
// bytecode instructions have nested expressions. This makes it easy to write an
// interpreter which is iterative, not recursive, which ultimately decouples the
// interpreter call stack from the call stack of the bl:mp program it is
// interpreting. This is advantageous because bl:mp programs make heavy use of
// recursion, and this property of the interpreter facilitates optimizations
// like tail call elimination.
//
// There are two data structures of importance when dealing with bytecode:
// Instruction, which represents a single bytecode instructions, and Bytecode,
// which represents a sequence of bytecode instructions, or a bytecode
// procedure.
//
////////////////////////////////////////////////////////////////////////////////
// instructions
//
//
// Bytecode instructions are represented using a variable-length encoding. Each
// instruction consists of a fixed-sized header (the Instruction type) followed
// by a variably-sized body. The header contains information common to all
// instructions, including the instruction opcode and the size of the
// instruction. The body contains operands specific to the opcode, and is
// represented by a subtype of Instruction specific to an opcode (e.g. the SYMI
// type for the INSTR_SYMI opcode). Since opcode types "inherit" from
// Instruction (that is, their first field is of type Instruction) they can be
// freely upcast to Instruction. Instructions can also be downcast to a specific
// opcode type, as long as the `type` field of the instruction header matches
// the target opcode type.
//
// In addition to operands encoded in the instruction itself, some instructions
// also operate on implicit operands from the result stack: every instruction
// which produces a result pushes its result onto a stack of object references
// called the result stack. Some instructions get their operands by popping
// object references off of the top of the result stack. This allows
// instructions to operate on dynamic data. For example, consider the SEND
// instruction: when the compiler translates a send expression, it first emits
// instructions which compute the receiver of the message, and then the message
// itself. These objects get stored on the result stack, so then the compiler
// can emit a simple SEND opcode, with no explicit operands, so that when the
// SEND instruction is executed, the interpreter can get the message and
// receiver from the stack. For example, the expression `foo bar` might get
// compiled as follows:
//
//      SYMI foo
//      SYMI bar
//      SEND
//
// For some instructions, it is optional whether the instruction places an
// object on the result stack. This is controlled by the `result_type` flag in
// the instruction header. A result type can be RESULT_USE, RESULT_IGNORE, or
// RESULT_INHERIT. RESULT_USE is the default, and it means that the instruction
// will always emit a result on the top of the result stack. RESULT_IGNORE can
// be used to execute an instruction for its side-effects, without actually
// computing the result. In the assembly mnemonics, this is denoted by a `v`
// (for "void") prefix before the opcode name. For example, the instruction `v
// SEND` will send the top of the result stack to the next object on the result
// stack, but the result of that send will be released rather than being pushed
// onto the result stack.
//
// The final result type is RESULT_INHERIT, which means that the result type of
// the instruction is dynamic; when the instruction is executed, it should
// inherit the result type from the caller. This result type is denoted by an
// `r` (for "return") prefix. For example, suppose we have the following section
// of bytecode:
//
//    r SYMI foo
//      RET
//
// If we jump to this code using `SEND`, then after it executes, the symbol
// `foo` will have been pushed onto the result stack, since the result type of
// `SEND` is RESULT_USE (the default), and the SYMI instruction inherits this
// result type. But if we execute this code using `v SEND`, then the result
// stack will not be affected, since the SYMI instruction inherits the
// RESULT_IGNORE result type.
//
// RESULT_INHERIT may be used to inherit from multiple levels up the call stack:
// if an instruction has result type RESULT_INHERIT, and the calling instruction
// also has result type RESULT_INHERIT, then the effective result type of the
// instruction is the result type of the caller's caller, and so on. The only
// rule is that when RESULT_INHERIT is used, there must be an instruction
// somewhere on the call stack whose result type is either RESULT_USE or
// RESULT_IGNORE.
//
////////////////////////////////////////////////////////////////////////////////
// procedures
//
// A bytecode procedure is a sequence of instructions which execute one after
// the other, usually terminating with a RET instruction which returns from the
// program. (Other instructions may be used to return, such as the tail call
// instruction RSEND.) A procedure may call other procedures. For example, when
// a SEND instruction executes where the receiver is a block object, the block's
// `code` procedure executes until a return instruction causes it to return to
// the procedure which executed the SEND.
//
// Procedures are represented by the Bytecode type. This type has an interface
// which facilitates building procedures (used by the compiler) and iterating
// over their instructions in order (used by the interpreter).
//

#ifndef BLIMP_INSTRUCTION_H
#define BLIMP_INSTRUCTION_H

#include "internal/bytecode.h"
#include "internal/object.h"

// Push a symbol immediate onto the result stack.
typedef struct {
    Instruction header;
    const Symbol *sym;
} SYMI;

typedef enum {
    BLOCK_DEFAULT = 0x0,
    BLOCK_CLOSURE = 0x1,
    BLOCK_LAMBDA  = 0x2,
} BlockFlags;

// Push a block immediate onto the result stack.
//
// The object will be a child of the scope currently on the call stack.
//
// The new object will capture a variable number of messages taken from the
// result stack (determined by `captures`) as well as the message being
// processed by the current frame on the call stack.
typedef struct {
    Instruction header;
    const Symbol *msg_name;
    Bytecode *code;
    BlockFlags flags;
    size_t captures;
} BLOCKI;

// Push a block immediate onto the result stack.
//
// The object will be a child of `scope`.
//
// The new object will capture a variable number of messages taken from the
// result stack (determined by `captures`), but not the message being processed
// by the current stack frame.
typedef struct {
    Instruction header;
    ScopedObject *scope;
    const Symbol *msg_name;
    Bytecode *code;
    BlockFlags flags;
    size_t captures;
} CLOSEI;

// Push a given object onto the result stack.
typedef struct {
    Instruction header;
    Object *object;
} OBJI;

// Get a captured message at the given index in the current scope, and push it
// onto the result stack.
typedef struct {
    Instruction header;
    size_t index;
} MSG;

// Get a captured message at the given index in the given scope, and push it
// onto the result stack.
typedef struct {
    Instruction header;
    ScopedObject *scope;
    size_t index;
} MSGOF;

typedef enum {
    SEND_DEFAULT = 0x0,
    SEND_TAIL    = 0x1,
} SendFlags;

// Push a new frame onto the call stack, where the message comes from the top of
// the result stack, the scope is the next obect from the result stack, and the
// return address is the address of the instruction after this one. Jump to the
// message handler of the scope.
typedef struct {
    Instruction header;
    SourceRange range;
    SendFlags flags;
    // message: stack
    // receiver: stack
} SEND;

// Same as SEND, except the receiving object is not stored on the result stack;
// it is stored with the instruction itself.
typedef struct {
    Instruction header;
    SourceRange range;
    Object *receiver;
    SendFlags flags;
    // message: stack
} SENDTO;

// Same as SEND, except instead have having an effect in the current scope, the
// effect if the receiver is a symbol is in the given scope.
typedef struct {
    Instruction header;
    SourceRange range;
    ScopedObject *scope;
    SendFlags flags;
    // message: stack
    // receiver: stack
} CALL;

// Same as SENDTO, except instead have having an effect in the current scope,
// the effect if the receiver is a symbol is in the given scope.
typedef struct {
    Instruction header;
    SourceRange range;
    ScopedObject *scope;
    Object *receiver;
    SendFlags flags;
    // message: stack
} CALLTO;

// Pop the current frame off of the call stack, and continue executing at the
// instruction saved in the `return_address` field. If the return address is
// NULL, terminate bytecode interpretation and return the top of the result
// stack as the overall result of the computation.
typedef struct {
    Instruction header;
} RET;

typedef struct {
    Instruction header;
} NOP;

#endif
