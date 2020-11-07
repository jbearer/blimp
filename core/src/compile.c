#include "internal/compile.h"
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
    BlockFlags flags)
{
    BLOCKI instr = {
        {INSTR_BLOCKI, result_type, sizeof(instr)},
        msg_name, block_code, flags, 0, 0
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
        {INSTR_RET, RESULT_IGNORE, sizeof(instr)}
    };

    return Bytecode_Append(code, (Instruction *)&instr);
}

////////////////////////////////////////////////////////////////////////////////
// Compilation logic
//

static Status CompileExpr(
    Blimp *blimp,
    Expr *expr,
    ResultType result_type,
    size_t depth,
    Bytecode *code);

static Status CompileStmt(
    Blimp *blimp,
    Expr *expr,
    ResultType result_type,
    SendFlags flags,
    size_t depth,
    bool *returned,
    Bytecode *code);

static Status CompileProcedure(
    Blimp *blimp, Expr *expr, size_t depth, Bytecode **code);

static Status CompileExpr(
    Blimp *blimp,
    Expr *expr,
    ResultType result_type,
    size_t depth,
    Bytecode *code)
{
    for (Expr *stmt = expr; stmt; stmt = stmt->next) {
        ResultType stmt_result_type = stmt->next ? RESULT_IGNORE : result_type;
            // The result of all statements except the last statement in the
            // sequence is ignored, so if there is another statement after this
            // one, we can ignore the result.
        TRY(CompileStmt(
            blimp, stmt, stmt_result_type, SEND_DEFAULT, depth, NULL, code));
    }

    return BLIMP_OK;
}

static Status CompileStmt(
    Blimp *blimp,
    Expr *stmt,
    ResultType result_type,
    SendFlags flags,
    size_t depth,
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
            TRY(CompileProcedure(
                blimp, stmt->block.code, depth + 1, &block_code));

            BlockFlags flags = BLOCK_LAMBDA;
            if (Expr_CapturesParentsMessage(stmt) != NO) {
                flags |= BLOCK_CLOSURE;
            }

            TRY(Emit_BLOCKI(
                code, result_type, stmt->block.msg_name, block_code, flags));
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
                        blimp, stmt->send.receiver, RESULT_IGNORE, depth, code));
                }
            } else {
                TRY(CompileExpr(blimp, stmt->send.receiver, RESULT_USE, depth, code));
            }

            // Emit instructions which cause the message to be the top object on
            // the result stack, and the receiver to be just below it, unless we
            // can determine that the receiver is a symbol literal, in which
            // case we wil optimize by setting the receiver object statically in
            // a SENDTO instruction.
            TRY(CompileExpr(blimp, stmt->send.message, RESULT_USE, depth, code));

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

static Status CompileProcedure(
    Blimp *blimp, Expr *expr, size_t depth, Bytecode **optimized)
{
    Bytecode *code;
    TRY(Bytecode_New(blimp, expr, &code));

    for (Expr *stmt = expr; stmt != NULL; stmt = stmt->next) {
        SendFlags flags = SEND_DEFAULT;
        if (stmt->next == NULL && blimp->options.tail_call_elimination) {
            flags |= SEND_TAIL;
        }

        ResultType result_type =
            stmt->next == NULL ? RESULT_INHERIT : RESULT_IGNORE;

        bool returned;
        if (CompileStmt(blimp, stmt, result_type, flags, depth, &returned, code)
                != BLIMP_OK)
        {
            BlimpBytecode_Free(code);
            return Reraise(blimp);
        }

        if (stmt->next == NULL && !returned) {
            if (Emit_RET(code) != BLIMP_OK) {
                BlimpBytecode_Free(code);
                return Reraise(blimp);
            }
        }
    }

    // Send the generated code through the optimizer, and return the new,
    // optimized procedure.
    if (Optimize(
            code, (ScopedObject *)blimp->global, depth, optimized)
        != BLIMP_OK)
    {
        BlimpBytecode_Free(code);
        return Reraise(blimp);
    }

    BlimpBytecode_Free(code);
        // Now that we're returning `optimized`, we're done with `code`.
    return BLIMP_OK;
}

Status BlimpExpr_Compile(Blimp *blimp, Expr *expr, Bytecode **code)
{
    TRY(Expr_Analyze(blimp, expr));
    return CompileProcedure(blimp, expr, 0, code);
}
