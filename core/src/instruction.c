#include <stddef.h>

#include "hash_set.h"

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

Status Bytecode_New(
    Blimp *blimp, Expr *expr, size_t specialized_seq, Bytecode **code)
{
    TRY(Malloc(blimp, sizeof(Bytecode), code));

    ++expr->refcount;

    (*code)->blimp = blimp;
    (*code)->expr = expr;
    (*code)->instructions = NULL;
    (*code)->size = 0;
    (*code)->capacity = 0;
    (*code)->refcount = 1;
    (*code)->specialized_seq = specialized_seq;
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

Status Bytecode_MoveToEnd(
    Bytecode *code, const Instruction *instr, size_t *new_offset)
{
    assert(Bytecode_Begin(code) <= instr && instr < Bytecode_End(code));
        // Make sure the instruction occurs within this array.
    assert((instr->size & (sizeof(void *) - 1)) == 0);
        // Make sure the size of the instruction is a multiple of the word size,
        // so that packing instructions together preserves alignment.
    size_t offset = (char *)instr - (char *)code->instructions;
        // Get the offset of the instruction so we have a stable way to
        // reference it before possibly reallocating the array.

    if (offset + instr->size == code->size) {
        // If this instruction is already the last instruction in the procedure,
        // we don't have to do anything.
        *new_offset = offset;
        return BLIMP_OK;
    } else {
        // If the instruction is not last in the procedure, we're going to move
        // it to the end, just past the current last in procedure.
        *new_offset = code->size;
    }

    // Increase the capacity of the instructions array if necessary.
    if (code->size + instr->size > code->capacity) {
        code->capacity = code->capacity*2 + instr->size;
            // We add `instr->size` to the new capacity so that the new array
            // is guaranteed to be able to hold the new instruction, even if the
            // old capacity is less than `instr->size / 2`.
        TRY(Realloc(code->blimp, code->capacity, &code->instructions));
    }

    // Copy the instruction from its current location to the end of the array.
    Instruction *src = (Instruction *)(
        (char *)code->instructions + offset);
    Instruction *dst = (Instruction *)(
        (char *)code->instructions + code->size);
    memcpy(dst, src, src->size);
    code->size += dst->size;

    // Replace the old instruction with a NOP.
    src->type = INSTR_NOP;
    src->result_type = RESULT_IGNORE;

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

static void PrintInstruction(
    FILE *file, const BlimpInstruction *ip, bool current)
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
    fprintf(file, " %s %p        %c ", current ? "=>" : "  ", ip, result_type);

    switch (ip->type) {
        case INSTR_BLOCKI: {
            BLOCKI *instr = (BLOCKI *)ip;
            PRINT_INSTR(file, BLOCKI, "%s, %p, %#x, %zu",
                instr->msg_name->name,
                instr->code,
                instr->flags,
                instr->captures);
            break;
        }

        case INSTR_CLOSEI: {
            CLOSEI *instr = (CLOSEI *)ip;
            PRINT_INSTR(file, CLOSEI, "%p, %s, %p, %#x, %zu",
                instr->scope,
                instr->msg_name->name,
                instr->code,
                instr->flags,
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
                    file, CALLTO, "%p, %s, %#x",
                    instr->scope,
                    ((const Symbol *)instr->receiver)->name,
                    instr->flags);
            } else {
                PRINT_INSTR(
                    file, CALLTO, "%p, %p, %#x",
                    instr->scope, instr->receiver, instr->flags);
            }
            break;
        }

        case INSTR_MACRO: {
            PRINT_INSTR(file, MACRO, "");
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
}

static void PrintProcedure(
    FILE *file,
    const BlimpBytecode *code,
    const Instruction *current,
    HashSet/*<Bytecode *>*/ *visited)
{
    bool recursive = visited != NULL;
    if (recursive) {
        if (HashSet_Contains(visited, &code)) {
            return;
        }
        CHECK(HashSet_Insert(visited, &code));
    }

    for (const Instruction *ip = Bytecode_Begin(code);
         ip != Bytecode_End(code);
         ip = Instruction_Next(ip))
    {
        PrintInstruction(file, ip, ip == current);

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
                    PrintProcedure(file, ((BLOCKI *)ip)->code, NULL, visited);
                    break;
                case INSTR_CLOSEI:
                    fputc('\n', file);
                    PrintProcedure(file, ((CLOSEI *)ip)->code, NULL, visited);
                    break;
                case INSTR_SENDTO: {
                    SENDTO *instr = (SENDTO *)ip;

                    // If this is a send to a symbol, dereference const symbols
                    // as long as we can. If we eventually prove the receiver is
                    // equalt to a block object, we can print the block's code.
                    ScopedObject *scope = (ScopedObject *)code->blimp->global;
                    Object *receiver = instr->receiver;
                    Object *value;
                    bool is_const;
                    while (
                        Object_Type(receiver) == OBJ_SYMBOL &&
                        ScopedObject_Lookup(
                            scope,
                            (const Symbol *)receiver,
                            &value,
                            NULL,
                            &is_const) &&
                        is_const
                    ) {
                        receiver = value;
                    }

                    // If the receiver is a block, print its code.
                    if (Object_Type(receiver) == OBJ_BLOCK) {
                        BlockObject *block = (BlockObject *)receiver;
                        if (Object_Type(instr->receiver) == OBJ_SYMBOL) {
                            fprintf(
                                file,
                                "  %p <%s>\n",
                                block->code,
                                ((const Symbol *)instr->receiver)->name
                            );
                        } else {
                            fprintf(file, "  %p <%p>\n", block->code, block);
                        }
                        PrintProcedure(file, block->code, NULL, visited);
                    }

                    break;
                }
                default:
                    break;
            }
        }
    }
}

void BlimpInstruction_Print(FILE *file, const Instruction *instr)
{
    PrintInstruction(file, instr, false);
}

void BlimpInstruction_PrintCurrent(FILE *file, const Instruction *instr)
{
    PrintInstruction(file, instr, true);
}

void BlimpBytecode_Print(FILE *file, const BlimpBytecode *code, bool recursive)
{
    BlimpBytecode_PrintWithIP(file, code, NULL, recursive);
}

static bool PointerEq(const void *p1, const void *p2, void *arg)
{
    (void)arg;
    return *(void **)p1 == *(void **)p2;
}

static size_t PointerHash(const void *p, void *arg)
{
    (void)arg;

    size_t hash = HASH_SEED;
    Hash_AddPointer(&hash, *(void **)p);
    return hash;
}

void BlimpBytecode_PrintWithIP(
    FILE *file,
    const BlimpBytecode *code,
    const Instruction *ip,
    bool recursive)
{
    HashSet/*<Bytecode *>*/ visited;
    if (recursive) {
        CHECK(HashSet_Init(
            code->blimp,
            &visited,
            sizeof(Bytecode *),
            PointerEq,
            PointerHash,
            NULL));
    }

    fprintf(file, "  %p\n", code);
    PrintProcedure(file, code, ip, recursive ? &visited : NULL);

    if (recursive) {
        HashSet_Destroy(&visited);
    }
}

static bool BlimpInstruction_Returns(const Instruction *instr)
{
    switch (instr->type) {
        case INSTR_SEND:
            return ((SEND *)instr)->flags & SEND_TAIL;
        case INSTR_SENDTO:
            return ((SENDTO *)instr)->flags & SEND_TAIL;
        case INSTR_CALL:
            return ((CALL *)instr)->flags & SEND_TAIL;
        case INSTR_CALLTO:
            return ((CALLTO *)instr)->flags & SEND_TAIL;
        case INSTR_RET:
            return true;
        default:
            return false;
    }
}

const Instruction *BlimpInstruction_Next(const Instruction *instr)
{
    if (BlimpInstruction_Returns(instr)) {
        // If the instruction would cause a return, it is the last instruction
        // in it's procedure and has no next instruction.
        return NULL;
    } else {
        return Instruction_Next(instr);
    }
}
