#ifndef BLIMP_BYTECODE_H
#define BLIMP_BYTECODE_H

#include "common.h"

////////////////////////////////////////////////////////////////////////////////
// Instructions
//

typedef enum {
    RESULT_USE,
    RESULT_IGNORE,
    RESULT_INHERIT,
} ResultType;

struct BlimpInstruction {
    enum {
        INSTR_SYMI,     // Symbol immediate
        INSTR_BLOCKI,   // Block immediate
        INSTR_CLOSEI,
        INSTR_OBJI,
        INSTR_MSG,      // Get captured message
        INSTR_MSGOF,
        INSTR_SEND,
        INSTR_SENDTO,
        INSTR_CALL,
        INSTR_CALLTO,
        INSTR_MACRO,
        INSTR_RET,
        INSTR_NOP,
    } type;

    ResultType result_type;
    size_t size;
};

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
    size_t specialized_seq;
        // Sequence number of the scope in which this code is specialized. The
        // object with this sequence number is an ancestor of all objects which
        // contain this code.

    Expr *expr;
        // The expression that generated this bytecode (for debugging only).
};

PRIVATE Status Bytecode_New(
    Blimp *blimp, Expr *expr, size_t specialized_seq, Bytecode **code);
PRIVATE Status Bytecode_Append(Bytecode *code, const Instruction *instr);
PRIVATE Status Bytecode_MoveToEnd(Bytecode *code, const Instruction *instr);
PRIVATE void Bytecode_Delete(Bytecode *code, Instruction *instr);
PRIVATE void Bytecode_RemoveNops(Bytecode *code);
PRIVATE void Bytecode_Truncate(Bytecode *code, size_t offset);

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

static inline size_t Bytecode_Offset(const Bytecode *code)
{
    return code->size;
}

static inline Instruction *Bytecode_Get(Bytecode *code, size_t offset)
{
    assert(offset < code->size);
    return (Instruction *)((char *)code->instructions + offset);
}

static inline bool Bytecode_Contains(
    const Bytecode *code, const Instruction *instr)
{
    return Bytecode_Begin(code) <= instr && instr < Bytecode_End(code);
}

#endif
