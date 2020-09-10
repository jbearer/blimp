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
        msg_name, block_code, capture_parents_message
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
    Bytecode *code, ResultType result_type, SourceRange range)
{
    SEND instr = {
        {INSTR_SEND, result_type, sizeof(instr)},
        range
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

// Returns `true` if `expr` definitely has no side-effects. This can allow
// certain expressions to be optimized out entirely.
static inline bool IsPure(Expr *expr)
{
    switch (expr->tag) {
        case EXPR_SYMBOL:
        case EXPR_BLOCK:
        case EXPR_MSG:
            return true;
        default:
            return false;
    }
}

// Append to `code` a sequence of instructions which has the effect of
// evaluating `expr` and pushing the result onto the result stack.
static Status CompileExpr(
    Blimp *blimp,
    Expr *expr,
    ResultType overall_result_type,
    Bytecode *code)
{
    for (Expr *stmt = expr; stmt; stmt = stmt->next) {
        // The result of all statements except the last statement in the
        // sequence is ignored, so if there is another statement after this one,
        // we can ignore the result.
        ResultType result_type =
            stmt->next ? RESULT_IGNORE : overall_result_type;

        // If we're ignoring the result, check if we can optimze out this
        // expression.
        if (result_type == RESULT_IGNORE && IsPure(stmt)) {
            continue;
        }

        switch (stmt->tag) {
            case EXPR_SYMBOL:
                TRY(Emit_SYMI(code, result_type, stmt->symbol));
                break;
            case EXPR_BLOCK: {
                // Compile the body of the block into its own separate
                // procedure.
                Bytecode *block_code;
                TRY(BlimpExpr_Compile(
                    blimp, stmt->block.code, &block_code));

                TRY(Emit_BLOCKI(
                    code,
                    result_type,
                    stmt->block.msg_name,
                    block_code,
                    stmt->block.captures_parents_message));
                break;
            }
            case EXPR_MSG:
                TRY(Emit_MSG(code, result_type, stmt->msg.index));
                break;
            case EXPR_SEND:
                // Emit instructions which cause the receiver and the message to
                // be the top two objects on the result stack.
                TRY(CompileExpr(blimp, stmt->send.receiver, RESULT_USE, code));
                TRY(CompileExpr(blimp, stmt->send.message, RESULT_USE, code));

                // The SEND instruction then operates implicitly on the top two
                // objects from the result stack.
                TRY(Emit_SEND(code, result_type, stmt->range));
                break;
        }
    }

    return BLIMP_OK;
}

Status BlimpExpr_Compile(Blimp *blimp, Expr *expr, Bytecode **code)
{
    TRY(Bytecode_New(blimp, expr, code));

    // Compile the expression, creating a sequence of instructions after which
    // the result of the procedure will be the top of the result stack.
    if (CompileExpr(blimp, expr, RESULT_INHERIT, *code) != BLIMP_OK) {
        BlimpBytecode_Free(*code);
        return Reraise(blimp);
    }

    // Terminate the procedure with a RET instruction.
    if (Emit_RET(*code) != BLIMP_OK) {
        BlimpBytecode_Free(*code);
        return Reraise(blimp);
    }

    return BLIMP_OK;
}

