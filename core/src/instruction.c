#include <stddef.h>

#include "internal/expr.h"
#include "internal/instruction.h"

static void Instruction_Destroy(const Instruction *instr)
{
    switch (instr->type) {
        case INSTR_BLOCKI:
            BlimpBytecode_Free(((BLOCKI *)instr)->code);
            break;
        case INSTR_CLOSEI:
            BlimpObject_Release((Object *)((CLOSEI *)instr)->scope);
            BlimpBytecode_Free(((CLOSEI *)instr)->code);
            break;
        case INSTR_OBJI:
            if (((OBJI *)instr)->object) {
                BlimpObject_Release(((OBJI *)instr)->object);
            }
            break;
        case INSTR_MSGOF:
            BlimpObject_Release((Object *)((MSGOF *)instr)->scope);
            break;
        case INSTR_SENDTO:
            BlimpObject_Release(((SENDTO *)instr)->receiver);
            break;
        case INSTR_CALL:
            BlimpObject_Release((Object *)((CALL *)instr)->scope);
            break;
        case INSTR_CALLTO:
            BlimpObject_Release((Object *)((CALLTO *)instr)->scope);
            BlimpObject_Release(((CALLTO *)instr)->receiver);
            break;
        default:
            break;
    }
}

Status Bytecode_New(Blimp *blimp, Expr *expr, Bytecode **code)
{
    TRY(Malloc(blimp, sizeof(Bytecode), code));

    ++expr->refcount;

    (*code)->blimp = blimp;
    (*code)->expr = expr;
    (*code)->instructions = NULL;
    (*code)->size = 0;
    (*code)->capacity = 0;
    (*code)->refcount = 1;
    return BLIMP_OK;
}

void BlimpBytecode_Free(Bytecode *code)
{
    if (--code->refcount > 0) {
        return;
    }

    Blimp_FreeExpr(code->expr);

    // Traverse the instructions, releasing references to other procedures in
    // BLOCKI operands and reference to objects in SENDTO operands.
    for (const Instruction *instr = Bytecode_Begin(code);
         instr != Bytecode_End(code);
         instr = Instruction_Next(instr))
    {
        Instruction_Destroy(instr);
    }

    Free(code->blimp, &code->instructions);
    Free(code->blimp, &code);
}

Status Bytecode_Append(Bytecode *code, const Instruction *instr)
{
    assert((instr->size & (sizeof(void *) - 1)) == 0);
        // Make sure the size of the instruction is a multiple of the word size,
        // so that packing instructions together preserves alignment.

    // Increase the capacity of the instructions array if necessary.
    if (code->size + instr->size > code->capacity) {
        code->capacity = code->capacity*2 + instr->size;
            // We add `instr->size` to the new capacity so that the new array
            // is guaranteed to be able to hold the new instruction, even if the
            // old capacity is less than `instr->size / 2`.
        TRY(Realloc(code->blimp, code->capacity, &code->instructions));
    }

    Instruction *dst = (Instruction *)(
        (char *)code->instructions + code->size);
    memcpy(dst, instr, instr->size);
    code->size += instr->size;

    return BLIMP_OK;
}

void Bytecode_Delete(Bytecode *code, Instruction *instr)
{
    assert(Bytecode_Begin(code) <= instr && instr < Bytecode_End(code));

    Instruction_Destroy(instr);
        // Release references to objects and other bytecode procedures owned by
        // the instruction we're deleting.

    if ((char *)instr + instr->size == (char *)code->instructions + code->size)
    {
        // If this instruction is the last instruction in the procedure, we can
        // delete it by simply shrinking the size of the procedure so it no
        // longer includes the last instruction.
        code->size -= instr->size;
    } else {
        // Otherwise, to have to actually delete the instruction, we'd need to
        // shift all of the following instructions backwards, which would be
        // expensive and would cause the offsets of instructions to change
        // unpredictably. Instead, we'll keep this instruction in place but
        // replace it with a NOP opcode. The caller can strip all NOPs out of
        // the procedur at once by calling Bytecode_RemoveNops() at a time when
        // they are ready for instruction offsets to change.
        instr->type = INSTR_NOP;
        instr->result_type = RESULT_IGNORE;
            // NOP instructions never produce a result, so we have to ignore the
            // non-existent result value.
    }
}

void Bytecode_RemoveNops(Bytecode *code)
{
    // We will traverse the instructions in `code` sequentially with two
    // cursors:
    Instruction *read = code->instructions;
        // `read` will move along one instruction at a time...
    Instruction *write = code->instructions;
        // `write` will trail behind it. We will copy instructions from `read`
        // to `write` only if they are not NOPs.

    while ((char *)read - (char *)code->instructions < (ptrdiff_t)code->size) {
        assert(write <= read);

        Instruction *next_read = (Instruction *)((char *)read + read->size);
            // Get the next value of `read` now, since copying from `read` to
            // `write` below might overwrite all or part of `read`, potentially
            // changing its `size`.

        if (read->type != INSTR_NOP) {
            if (write != read) {
                // Copy non-NOP instruction from `read` to `write`, but only if
                // `read` and `write` refer to different addresses (otherwise a
                // copy would be redundant).
                memmove(write, read, read->size);
            }
            write = (Instruction *)((char *)write + write->size);
                // Advance write.
        } else {
            // Don't copy the instruction or advance the `write` pointer if the
            // instruction is a NOP.
        }

        read = next_read;
    }

    code->size = (char *)write - (char *)code->instructions;
        // Recompute size.
}

void Bytecode_Truncate(Bytecode *code, size_t offset)
{
    if (offset >= code->size) {
        return;
    }

    // Clean up references owned by instructions between the new end of the
    // procedure and the current end.
    const Instruction *ip = Bytecode_Get(code, offset);
    while (ip < Bytecode_End(code)) {
        const Instruction *next = Instruction_Next(ip);
        Instruction_Destroy(ip);
        ip = next;
    }

    code->size = offset;
}

#define PRINT_INSTR(file, mnemonic, fmt, ...) \
    fprintf(file, "%-8s" fmt "\n", #mnemonic, ##__VA_ARGS__)

void BlimpBytecode_Print(FILE *file, const BlimpBytecode *code, bool recursive)
{
    fprintf(file, "  %p\n", code);

    for (const Instruction *ip = Bytecode_Begin(code);
         ip != Bytecode_End(code);
         ip = Instruction_Next(ip))
    {
        // Print a single-character prefix indicating the result type.
        char result_type;
        switch (ip->result_type) {
            case RESULT_INHERIT:
                result_type = 'r';
                break;
            case RESULT_IGNORE:
                result_type = 'v';
                break;
            default:
                result_type = ' ';
                break;
        }
        fprintf(file, "    %c ", result_type);

        switch (ip->type) {
            case INSTR_BLOCKI: {
                BLOCKI *instr = (BLOCKI *)ip;
                PRINT_INSTR(file, BLOCKI, "%s, %p, %#x, %zu, %zu",
                    instr->msg_name->name,
                    instr->code,
                    instr->flags,
                    instr->specialized,
                    instr->captures);
                break;
            }

            case INSTR_CLOSEI: {
                CLOSEI *instr = (CLOSEI *)ip;
                PRINT_INSTR(file, CLOSEI, "%p, %s, %p, %#x, %zu, %zu",
                    instr->scope,
                    instr->msg_name->name,
                    instr->code,
                    instr->flags,
                    instr->specialized,
                    instr->captures);
                break;
            }

            case INSTR_SYMI: {
                SYMI *instr = (SYMI *)ip;
                PRINT_INSTR(file, SYMI, "%s", instr->sym->name);
                break;
            }

            case INSTR_OBJI: {
                OBJI *instr = (OBJI *)ip;
                PRINT_INSTR(file, OBJI, "%p", instr->object);
                break;
            }

            case INSTR_MSG: {
                MSG *instr = (MSG *)ip;
                PRINT_INSTR(file, MSG, "%zu", instr->index);
                break;
            }

            case INSTR_MSGOF: {
                MSGOF *instr = (MSGOF *)ip;
                PRINT_INSTR(file, MSGOF, "%p, %zu", instr->scope, instr->index);
                break;
            }

            case INSTR_SEND: {
                SEND *instr = (SEND *)ip;
                PRINT_INSTR(file, SEND, "%#x", instr->flags);
                break;
            }

            case INSTR_CALL: {
                CALL *instr = (CALL *)ip;
                PRINT_INSTR(file, CALL, "%p, %#x", instr->scope, instr->flags);
                break;
            }

            case INSTR_SENDTO: {
                SENDTO *instr = (SENDTO *)ip;
                if (Object_Type(instr->receiver) == OBJ_SYMBOL) {
                    PRINT_INSTR(
                        file, SENDTO, "%s, %#x",
                        ((const Symbol *)instr->receiver)->name, instr->flags);
                } else {
                    PRINT_INSTR(
                        file, SENDTO, "%p, %#x", instr->receiver, instr->flags);
                }
                break;
            }

            case INSTR_CALLTO: {
                CALLTO *instr = (CALLTO *)ip;
                if (Object_Type(instr->receiver) == OBJ_SYMBOL) {
                    PRINT_INSTR(
                        file, SENDTO, "%p, %s, %#x",
                        instr->scope,
                        ((const Symbol *)instr->receiver)->name,
                        instr->flags);
                } else {
                    PRINT_INSTR(
                        file, SENDTO, "%p, %p, %#x",
                        instr->scope, instr->receiver, instr->flags);
                }
                break;
            }

            case INSTR_RET: {
                PRINT_INSTR(file, RET, "");
                break;
            }

            case INSTR_NOP: {
                PRINT_INSTR(file, NOP, "%zu", ip->size);
                break;
            }

            default: {
                PRINT_INSTR(file, ???, "");
                break;
            }
        }

        if (ip->type == INSTR_RET) {
            break;
        }
    }

    if (recursive) {
        // Traverse the instructions, looking for BLOCKI instructions which
        // reference other procedures, and printing any procedures we find.
        for (const Instruction *ip = Bytecode_Begin(code);
             ip != Bytecode_End(code);
             ip = Instruction_Next(ip))
        {
            switch (ip->type) {
                case INSTR_BLOCKI:
                    fputc('\n', file);
                    BlimpBytecode_Print(file, ((BLOCKI *)ip)->code, true);
                    break;
                case INSTR_CLOSEI:
                    fputc('\n', file);
                    BlimpBytecode_Print(file, ((CLOSEI *)ip)->code, true);
                    break;
                default:
                    break;
            }
        }
    }
}
