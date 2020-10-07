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

#include "internal/common.h"

////////////////////////////////////////////////////////////////////////////////
// Instructions
//

typedef enum {
    RESULT_USE,
    RESULT_IGNORE,
    RESULT_INHERIT,
} ResultType;

typedef struct {
    enum {
        INSTR_SYMI,     // Symbol immediate
        INSTR_BLOCKI,   // Block immediate
        INSTR_MSG,      // Get captured message
        INSTR_SEND,
        INSTR_SENDTO,
        INSTR_RET,
    } type;

    ResultType result_type;
    size_t size;
} Instruction;

// Push a symbol immediate onto the result stack.
typedef struct {
    Instruction header;
    const Symbol *sym;
} SYMI;

// Push a block immediate onto the result stack.
typedef struct {
    Instruction header;
    const Symbol *msg_name;
    Bytecode *code;
    bool capture_parents_message;
    size_t specialized;
} BLOCKI;

// Get a captured message at the given index in the current scope, and push it
// onto the result stack.
typedef struct {
    Instruction header;
    size_t index;
} MSG;

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
    SendFlags flags;
    Object *receiver;
    // message: stack
} SENDTO;

// Pop the current frame off of the call stack, and continue executing at the
// instruction saved in the `return_address` field. If the return address is
// NULL, terminate bytecode interpretation and return the top of the result
// stack as the overall result of the computation.
typedef struct {
    Instruction header;
} RET;

////////////////////////////////////////////////////////////////////////////////
// Bytecode programs
//

struct BlimpBytecode {
    Blimp *blimp;
    size_t refcount;

    // A procedure is essentially a vector of instructions, representing using a
    // pointer to the beginning, a capacity, and a size.
    void *instructions;
        // The data array of the vector is conceptually an array of
        // Instructions; however, we use `void *` to represent it, since
        // instructions are variably-sized and thus we do not support random
        // access via normal array indexing. The only way to know which offsets
        // within this array represent the starts of instructions is to traverse
        // the array, starting from `instructions` and offsetting by
        // `instr->size` bytes for each subsequent instruction `instr`.
    size_t capacity;
        // The size in bytes of the contiguously allocated array pointed to by
        // `instructions`.
    size_t size;
        // The size in bytes of the prefix of `instructions` which contains
        // valid data.

    Expr *expr;
        // The expression that generated this bytecode (for debugging only).
};

PRIVATE Status Bytecode_New(Blimp *blimp, Expr *expr, Bytecode **code);
PRIVATE Status Bytecode_Append(Bytecode *code, const Instruction *instr);

// Get the first instruction in a procedure.
static inline const Instruction *Bytecode_Begin(const Bytecode *code)
{
    return (Instruction *)code->instructions;
}

// Get a pointer to the first instruction past the last instruction in a
// procedure. This pointer may not be dereferenced; however, it is guaranteed to
// compare equal to Instruction_Next of the last dereferencable pointer (which
// is usually a RET, but procedures in the process of being compiled may not
// have a terminating RET added yet).
static inline const Instruction *Bytecode_End(const Bytecode *code)
{
    return (Instruction *)((char *)code->instructions + code->size);
}

static inline const Instruction *Instruction_Next(const Instruction *instr)
{
    return (Instruction *)((char *)instr + instr->size);
}

static inline Expr *Bytecode_Expr(const Bytecode *code)
{
    return code->expr;
}

#endif
