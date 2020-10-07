#include "internal/expr.h"
#include "internal/instruction.h"

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
        switch (instr->type) {
            case INSTR_BLOCKI:
                BlimpBytecode_Free(((BLOCKI *)instr)->code);
                break;
            case INSTR_SENDTO:
                BlimpObject_Release(((SENDTO *)instr)->receiver);
                break;
            default:
                break;
        }
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
                PRINT_INSTR(file, BLOCKI, "%s, %p, %d, %zu",
                    instr->msg_name->name,
                    instr->code,
                    instr->capture_parents_message,
                    instr->specialized);
                break;
            }

            case INSTR_SYMI: {
                SYMI *instr = (SYMI *)ip;
                PRINT_INSTR(file, SYMI, "%s", instr->sym->name);
                break;
            }

            case INSTR_MSG: {
                MSG *instr = (MSG *)ip;
                PRINT_INSTR(file, MSG, "%zu", instr->index);
                break;
            }

            case INSTR_SEND: {
                SEND *instr = (SEND *)ip;
                PRINT_INSTR(file, SEND, "%#x", instr->flags);
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

            case INSTR_RET: {
                PRINT_INSTR(file, RET, "");
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
            if (ip->type == INSTR_BLOCKI) {
                fputc('\n', file);
                BlimpBytecode_Print(file, ((BLOCKI *)ip)->code, true);
            } else if (ip->type == INSTR_RET) {
                break;
            }
        }
    }
}
