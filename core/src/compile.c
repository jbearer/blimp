#include "internal/expr.h"
#include "internal/instruction.h"

////////////////////////////////////////////////////////////////////////////////
// Helper functions for appending instructions to a Bytecode procedure
//

static inline Status Emit_SYMI(
    Bytecode *code, ResultType result_type, const Symbol *symbol)
{
    SYMI instr = {
        {INSTR_SYMI, result_type, sizeof(instr)},
        symbol
    };

    return Bytecode_Append(code, (Instruction *)&instr);
}

static inline Status Emit_BLOCKI(
    Bytecode *code,
    ResultType result_type,
    const Symbol *msg_name,
    Bytecode *block_code,
    bool capture_parents_message)
{
    BLOCKI instr = {
        {INSTR_BLOCKI, result_type, sizeof(instr)},
        msg_name, block_code, capture_parents_message, 0
    };

    return Bytecode_Append(code, (Instruction *)&instr);
}

static inline Status Emit_MSG(
    Bytecode *code, ResultType result_type, size_t index)
{
    MSG instr = {
        {INSTR_MSG, result_type, sizeof(instr)},
        index
    };

    return Bytecode_Append(code, (Instruction *)&instr);
}

static inline Status Emit_SEND(
    Bytecode *code, ResultType result_type, SourceRange range, SendFlags flags)
{
    SEND instr = {
        {INSTR_SEND, result_type, sizeof(instr)},
        range, flags
    };

    return Bytecode_Append(code, (Instruction *)&instr);
}

static inline Status Emit_SENDTO(
    Bytecode *code,
    ResultType result_type,
    SourceRange range,
    SendFlags flags,
    Object *receiver)
{
    SENDTO instr = {
        {INSTR_SENDTO, result_type, sizeof(instr)},
        range, flags, receiver
    };

    return Bytecode_Append(code, (Instruction *)&instr);
}

static inline Status Emit_RET(Bytecode *code)
{
    RET instr = {
        {INSTR_RET, RESULT_USE, sizeof(instr)}
    };

    return Bytecode_Append(code, (Instruction *)&instr);
}

////////////////////////////////////////////////////////////////////////////////
// Compilation logic
//

static Status CompileExpr(
    Blimp *blimp,Expr *expr, ResultType result_type, Bytecode *code);

static Status CompileStmt(
    Blimp *blimp,
    Expr *expr,
    ResultType result_type,
    SendFlags flags,
    bool *returned,
    Bytecode *code);

static Status CompileProcedure(Blimp *blimp, Expr *expr, Bytecode *code);

static Status CompileExpr(
    Blimp *blimp, Expr *expr, ResultType result_type, Bytecode *code)
{
    for (Expr *stmt = expr; stmt; stmt = stmt->next) {
        ResultType stmt_result_type = stmt->next ? RESULT_IGNORE : result_type;
            // The result of all statements except the last statement in the
            // sequence is ignored, so if there is another statement after this
            // one, we can ignore the result.
        TRY(CompileStmt(
            blimp, stmt, stmt_result_type, SEND_DEFAULT, NULL, code));
    }

    return BLIMP_OK;
}

static Status CompileStmt(
    Blimp *blimp,
    Expr *stmt,
    ResultType result_type,
    SendFlags flags,
    bool *returned,
    Bytecode *code)
{
    if (returned) {
        *returned = false;
    }

    // If we're ignoring the result, check if we can optimze out this
    // expression.
    if (result_type == RESULT_IGNORE && Stmt_IsPure(stmt) == YES) {
        return BLIMP_OK;
    }

    switch (stmt->tag) {
        case EXPR_SYMBOL:
            TRY(Emit_SYMI(code, result_type, stmt->symbol));
            break;
        case EXPR_BLOCK: {
            // Compile the body of the block into its own separate
            // procedure.
            Bytecode *block_code;
            TRY(Bytecode_New(blimp, stmt->block.code, &block_code));
            if (CompileProcedure(blimp, stmt->block.code, block_code)
                    != BLIMP_OK)
            {
                BlimpBytecode_Free(block_code);
                return Reraise(blimp);
            }

            TRY(Emit_BLOCKI(
                code,
                result_type,
                stmt->block.msg_name,
                block_code,
                Expr_CapturesParentsMessage(stmt) != NO));
            break;
        }
        case EXPR_MSG:
            TRY(Emit_MSG(code, result_type, stmt->msg.index));
            break;
        case EXPR_SEND: {
            const Symbol *sym_receiver = NULL;
            if (Expr_EvaluatesToSymbol(
                    stmt->send.receiver, &sym_receiver) == YES)
            {
                if (Expr_IsPure(stmt->send.receiver) != YES) {
                    TRY(CompileExpr(
                        blimp, stmt->send.receiver, RESULT_IGNORE, code));
                }
            } else {
                TRY(CompileExpr(blimp, stmt->send.receiver, RESULT_USE, code));
            }

            // Emit instructions which cause the message to be the top object on
            // the result stack, and the receiver to be just below it, unless we
            // can determine that the receiver is a symbol literal, in which
            // case we wil optimize by setting the receiver object statically in
            // a SENDTO instruction.
            TRY(CompileExpr(blimp, stmt->send.message, RESULT_USE, code));

            if (sym_receiver == NULL) {
                // The SEND instruction then operates implicitly on the top two
                // objects from the result stack.
                TRY(Emit_SEND(code, result_type, stmt->range, flags));
            } else {
                TRY(Emit_SENDTO(
                    code,
                    result_type,
                    stmt->range,
                    flags,
                    (Object *)sym_receiver
                ));
            }

            if (returned) {
                *returned = !!(flags & SEND_TAIL);
            }
            break;
        }
    }

    return BLIMP_OK;
}

static Status CompileProcedure(Blimp *blimp, Expr *expr, Bytecode *code)
{
    for (Expr *stmt = expr; stmt != NULL; stmt = stmt->next) {
        SendFlags flags = SEND_DEFAULT;
        if (stmt->next == NULL && blimp->options.tail_call_elimination) {
            flags |= SEND_TAIL;
        }

        ResultType result_type =
            stmt->next == NULL ? RESULT_INHERIT : RESULT_IGNORE;

        bool returned;
        TRY(CompileStmt(blimp, stmt, result_type, flags, &returned, code));

        if (stmt->next == NULL && !returned) {
            TRY(Emit_RET(code));
        }
    }

    return BLIMP_OK;
}

Status BlimpExpr_Compile(Blimp *blimp, Expr *expr, Bytecode **code)
{
    TRY(Expr_Analyze(blimp, expr));
    TRY(Bytecode_New(blimp, expr, code));

    if (CompileProcedure(blimp, expr, *code) != BLIMP_OK) {
        BlimpBytecode_Free(*code);
        return Reraise(blimp);
    }

    return BLIMP_OK;
}

////////////////////////////////////////////////////////////////////////////////
// Bytecode optimizer
//

static Status OptimizeInstructionForScope(
    Blimp *blimp,
    const Instruction *ip,
    ScopedObject *scope,
    Bytecode *replace_subroutine,
    Bytecode *optimized_subroutine,
    size_t specialized,
    Bytecode *optimized)
{
    switch (ip->type) {
        case INSTR_BLOCKI: {
            BLOCKI *instr = (BLOCKI *)ip;
            if (instr->code == replace_subroutine) {
                BLOCKI new_instr = *instr;

                ++optimized_subroutine->refcount;
                new_instr.code = optimized_subroutine;
                new_instr.specialized = specialized;

                if (Bytecode_Append(
                        optimized, (Instruction *)&new_instr) != BLIMP_OK)
                {
                    BlimpBytecode_Free(optimized_subroutine);
                    return Reraise(blimp);
                }
            } else {
                ++instr->code->refcount;
                if (Bytecode_Append(optimized, ip) != BLIMP_OK) {
                    BlimpBytecode_Free(instr->code);
                    return Reraise(blimp);
                }
            }

            break;
        }

        case INSTR_SENDTO: {
            SENDTO *instr = (SENDTO *)ip;
            SENDTO new_instr = *instr;

            BlimpObject_Borrow(new_instr.receiver);

            bool is_const;
            ScopedObject *owner;
            Object *new_receiver;
            while (
                Object_Type(new_instr.receiver) == OBJ_SYMBOL &&
                ScopedObject_Lookup(
                    scope,
                    (const Symbol *)new_instr.receiver,
                    &new_receiver,
                    &owner,
                    &is_const) &&
                is_const &&
                owner->seq <= scope->seq
            ) {
                BlimpObject_Release(new_instr.receiver);
                new_instr.receiver = BlimpObject_Borrow(new_receiver);
            }

            TRY(Bytecode_Append(optimized, (Instruction *)&new_instr));

            break;
        }

        default:
            TRY(Bytecode_Append(optimized, ip));
    }

    return BLIMP_OK;
}

Status OptimizeForScope(
    Bytecode *code,
    ScopedObject *scope,
    Bytecode *replace_subroutine,
    Bytecode *optimized_subroutine,
    size_t specialized,
    Bytecode **optimized)
{
    Blimp *blimp = code->blimp;

    TRY(Bytecode_New(blimp, code->expr, optimized));

    for (const Instruction *ip = Bytecode_Begin(code);
         ip != Bytecode_End(code);
         ip = Instruction_Next(ip))
    {
        if (OptimizeInstructionForScope(
                blimp,
                ip,
                scope,
                replace_subroutine,
                optimized_subroutine,
                specialized,
                *optimized)
            != BLIMP_OK)
        {
            BlimpBytecode_Free(*optimized);
            return Reraise(blimp);
        }
    }

    return BLIMP_OK;
}
