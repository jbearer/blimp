#include "internal/blimp.h"
#include "internal/optimizer.h"

static inline Status SymbolicObject_New(Optimizer *opt, SymbolicObject **obj)
{
    Blimp *blimp = Optimizer_Blimp(opt);

    *obj = CheckpointAllocator_Alloc(&opt->sym_objects, sizeof(SymbolicObject));
    if (*obj == NULL) {
        return Error(blimp, BLIMP_OUT_OF_MEMORY);
    }

    // Initialize the object into a trivial state, where we don't statically
    // know anything about this object (not what it's value is or where it came
    // from). The caller can add more information as it becomes available.
    memset(*obj, 0, sizeof(**obj));
    (*obj)->instr_offset = UNKNOWN_INSTR_OFFSET;
    (*obj)->value_type = VALUE_UNKNOWN;

    return BLIMP_OK;
}

Status SymbolicObject_Copy(
    Optimizer *opt, const SymbolicObject *src, SymbolicObject *dst)
{
    SymbolicObject_CopyValue(src, dst);
        // Copy the symbolic representation of the object's value.

    if (src->value_type == VALUE_LAMBDA) {
        // If the object has a list of captures, make a deep copy of the
        // captures list. Each object in `dst`s captures will have the same
        // value_type and value as the corresponding object in `src`s captures,
        // but they will be different objects, as the objects captured by `src`
        // will not necessarily be on the stack if and when `dst` is evaluated.
        dst->captures = NULL;
        SymbolicObject *dst_captures_end = NULL;

        for (SymbolicObject *src_capture = src->captures;
             src_capture != NULL;
             src_capture = src_capture->next)
        {
            SymbolicObject *dst_capture;
            TRY(SymbolicObject_New(opt, &dst_capture));
            TRY(SymbolicObject_Copy(opt, src_capture, dst_capture));

            if (dst_captures_end == NULL) {
                dst->captures = dst_capture;
            } else {
                dst_captures_end->next = dst_capture;
            }
            dst_captures_end = dst_capture;
        }
    }

    return BLIMP_OK;
}

Status Optimizer_SymbolizeObject(
    Optimizer *opt, Object *obj, SymbolicObject **result)
{
    TRY(SymbolicObject_New(opt, result));

    if (Object_Type(obj) == OBJ_SYMBOL) {
        (*result)->value_type = VALUE_SYMBOL;
        (*result)->value.symbol = (const Symbol *)obj;
    } else {;
        (*result)->value_type = VALUE_OBJECT;
        (*result)->value.object = obj;
    }

    return BLIMP_OK;
}

Status Optimizer_Init(Blimp *blimp, Optimizer *opt)
{
    TRY(SymbolicObjectStack_Init(
        blimp,
        &opt->result_stack,
        blimp->options.recursion_limit*16
            // We assume (somewhat arbitrarily) that there will be an average of
            // 16 objects on the stack at a time per call stack frame. Errors in
            // the size of the object stack will be caught and turned into
            // BLIMP_STACK_OVERFLOW runtime errors.
    ));
    DBMap_Init(blimp, &opt->messages);
    if (CheckpointAllocator_Init(
            blimp, &opt->sym_objects,

            // For the block size, we use the `gc_batch_size` option rounded up
            // to a multiple of `sizeof(SymbolicObject)`.
            ((blimp->options.gc_batch_size/sizeof(SymbolicObject))+1) *
                sizeof(SymbolicObject))
        != BLIMP_OK)
    {
        SymbolicObjectStack_Destroy(blimp, &opt->result_stack);
        return Reraise(blimp);
    }

    opt->code = NULL;
    opt->stack = NULL;

    return BLIMP_OK;
}

void Optimizer_Destroy(Optimizer *opt)
{
    Blimp *blimp = Optimizer_Blimp(opt);

    CheckpointAllocator_Destroy(&opt->sym_objects);
    DBMap_Destroy(&opt->messages);
    SymbolicObjectStack_Destroy(blimp, &opt->result_stack);
    CheckpointAllocator_Destroy(&opt->sym_objects);
}

Status Optimizer_Begin(
    Optimizer *opt,
    ScopedObject *scope,
    size_t depth,
    Expr *expr,
    Bytecode *replace_subroutine,
    Bytecode *optimized_subroutine,
    size_t specialized)
{
    Blimp *blimp = Object_Blimp((Object *)scope);

    assert(opt->code == NULL);
    assert(SymbolicObjectStack_Empty(&opt->result_stack));
    assert(DBMap_Empty(&opt->messages));
    assert(opt->stack == NULL);

    opt->replace_subroutine = replace_subroutine;
    opt->optimized_subroutine = optimized_subroutine;
    opt->specialized = specialized;
    opt->ghost_objects = 0;

    if (Bytecode_New(blimp, expr, specialized, &opt->code) != BLIMP_OK) {
        Optimizer_End(opt, NULL);
        return Reraise(blimp);
    }

    // For each message captured by `scope`, create a corresponding
    // SymbolicObject in the optimizer's message stack.
    DeBruijnMap *inherited_messages = &scope->captures;
    ScopedObject *owner = scope;
        // The object which owns the current capture.
    if (DBMap_Fill(&opt->messages, DBMap_Size(inherited_messages), NULL)
            != BLIMP_OK)
    {
        Optimizer_End(opt, NULL);
        return Reraise(blimp);
    }
    for (size_t i = 0; i < DBMap_Size(inherited_messages); ++i) {
        SymbolicObject *obj;
        if (SymbolicObject_New(opt, &obj) != BLIMP_OK) {
            Optimizer_End(opt, NULL);
            return Reraise(blimp);
        }

        if (owner->seq <= opt->specialized) {
            // If the capture is owned by an object which is an ancestor of the
            // scope of specification, then we can assume the capture with this
            // index will always have the same value when referenced from the
            // optimized code, since the optimized code always runs in the scope
            // of specialization or one of its descendants. Therefore, the
            // corresponding SymbolicObject should have value type
            // `VALUE_OBJECT` (or `VALUE_SYMBOL`) to indicate that we know
            // exactly which object it represents.
            obj->value_type = VALUE_OBJECT;

            // Get the value of the object from `scope`s captured messages.
            Ref *ref = DBMap_Resolve(inherited_messages, i);
            if (ref) {
                if (Object_Type(ref->to) == OBJ_SYMBOL) {
                    obj->value_type = VALUE_SYMBOL;
                    obj->value.symbol = (const Symbol *)ref->to;
                } else {
                    obj->value.object = ref->to;
                }
            }
        }

        // Add the SymbolicObject to the symbolic stack of captured messages.
        DBMap_Set(&opt->messages, i, obj);

        owner = owner->parent;
    }

    // Each lexical scope between `scope` and the object we're optimizing
    // represents one object whose message we will capture at runtime. Since we
    // are not privy to the details of these intervening scopes at optimization
    // time, we can't say anything about their messages, so we will represent
    // these captured messages with SymbolicObjects with value type
    // `VALUE_UNKNOWN`.
    for (size_t i = 0; i < depth + 1; ++i) {
        SymbolicObject *obj;
        if (SymbolicObject_New(opt, &obj) != BLIMP_OK) {
            Optimizer_End(opt, NULL);
            return Reraise(blimp);
        }
        if (DBMap_Push(&opt->messages, obj) != BLIMP_OK) {
            Optimizer_End(opt, NULL);
            return Reraise(blimp);
        }
    }

    return BLIMP_OK;
}

void Optimizer_End(Optimizer *opt, Bytecode **code)
{
    assert(SymbolicObjectStack_Empty(&opt->result_stack));
    assert(opt->stack == NULL);

    if (code) {
        // If the caller wants us to return `code`, then first clean it up for
        // them:
        Bytecode_RemoveNops(opt->code);
            // There may be NOPs in the generated code from places where we
            // deleted instructions. Get rid of all of those now.
        *code = opt->code;
    } else if (opt->code) {
        // Otherwise we can get rid of `code`.
        BlimpBytecode_Free(opt->code);
    }

    // Clean up resources.
    DBMap_Clear(&opt->messages);
    CheckpointAllocator_Clear(&opt->sym_objects);

    opt->code = NULL;
    opt->replace_subroutine = NULL;
    opt->optimized_subroutine = NULL;
}

static Status Emit(
    Optimizer *opt,
    const Instruction *instr,
    bool move,
    SymbolicObject **result)
{
    Blimp *blimp = Optimizer_Blimp(opt);

    TRY(SymbolicObject_New(opt, result));
    opt->last_offset = Bytecode_Offset(opt->code);
    (*result)->instr_offset = opt->last_offset;

    if (move) {
        if (Bytecode_MoveToEnd(opt->code, instr) != BLIMP_OK) {
            return Reraise(blimp);
        }
        instr = Bytecode_Get(opt->code, (*result)->instr_offset);
    } else {
        if (Bytecode_Append(opt->code, instr) != BLIMP_OK) {
            return Reraise(blimp);
        }
    }

    // Update the `result_type` of the new instruction if necessary.
    if (instr->result_type == RESULT_INHERIT &&
            // If `instr->result_type` is `RESULT_INHERIT`, this indicates that
            // `instr` may or may not produce a result depending on whether the
            // caller of this procedure requested one.
        opt->stack != NULL &&
            // We could just emit an instruction with the same `RESULT_INHERIT`
            // property, unless we are inlining, in which case the "caller" if
            // the procedure being inlined is this procedure.
        !opt->stack->tail_call
            // In that case, we look at the inlined stack frame to see if the
            // call being inlined is in a tail position. If it is, then the
            // result of the inlined instruction will be the result of the
            // overall procedure, and so `RESULT_INHERIT` is correct. But if
            // it's not, we need to set a definitive `result_type` based on
            // whether the send being inlined was supposed to produce a result
            // or not.
    ) {
        // If the send being inlined was supposed to produce a result (indicated
        // by the `use_result` field of the stack frame) then the new result
        // type becomes `RESULT_USE`. Otherwise, it is `RESULT_IGNORE`.
        Bytecode_Get(opt->code, (*result)->instr_offset)->result_type =
            opt->stack->use_result ? RESULT_USE : RESULT_IGNORE;
    }

    if (Optimizer_UseResult(opt, instr->result_type)) {
        // If the adjusted result type indicates that this instruction should
        // produce a result, then push the result onto the result stack.
        if (SymbolicObjectStack_Push(
                blimp, &opt->result_stack, *result) != BLIMP_OK)
        {
            Bytecode_Delete(
                opt->code, Bytecode_Get(opt->code, (*result)->instr_offset));
            return Reraise(blimp);
        }
    }

    return BLIMP_OK;
}

Status Optimizer_Emit(
    Optimizer *opt, const Instruction *instr, SymbolicObject **result)
{
    return Emit(opt, instr, false, result);
}

Status Optimizer_EmitGhost(
    Optimizer *opt, ResultType result_type, SymbolicObject **result)
{
    Blimp *blimp = Optimizer_Blimp(opt);

    TRY(SymbolicObject_New(opt, result));
    (*result)->instr_offset = GHOST_INSTR_OFFSET;

    if (Optimizer_UseResult(opt, result_type)) {
        // If the result type indicates that this instruction should produce a
        // result, then push the result onto the result stack.
        if (SymbolicObjectStack_Push(
                blimp, &opt->result_stack, *result) != BLIMP_OK)
        {
            return Reraise(blimp);
        }
    }

    ++opt->ghost_objects;
    return BLIMP_OK;
}

void Optimizer_Delete(Optimizer *opt, SymbolicObject *obj)
{
    if (obj->instr_offset == GHOST_INSTR_OFFSET) {
        // If this is a ghost object, decrement the count of outstanding objects
        // and clear the ghostly status of the object.
        assert(opt->ghost_objects > 0);
        --opt->ghost_objects;
        obj->instr_offset = UNKNOWN_INSTR_OFFSET;
    }

    if (obj->instr_offset != UNKNOWN_INSTR_OFFSET) {
        // If the object corresponds to a known instruction, we will try to
        // delete that instruction from the generated code.
        Instruction *instr = Bytecode_Get(opt->code, obj->instr_offset);

        bool is_pure;
        switch (instr->type) {
            case INSTR_SYMI:
            case INSTR_BLOCKI:
            case INSTR_CLOSEI:
            case INSTR_OBJI:
            case INSTR_MSG:
            case INSTR_MSGOF:
                is_pure = true;
                break;
            default:
                is_pure = false;
                break;
        }

        if (is_pure) {
            // We can only delete the instruction without changing the meaning
            // of the program if it is pure (that is, has no side-effects).

            // Recursively delete inputs which were pushed onto the stack just
            // to be consumed by the instruction which we're now deleting.
            if (instr->type == INSTR_BLOCKI || instr->type == INSTR_CLOSEI) {
                for (SymbolicObject *capture = obj->captures;
                     capture != NULL;
                     capture = capture->next)
                {
                    Optimizer_Delete(opt, capture);
                }
            } else {
                if (obj->receiver) {
                    Optimizer_Delete(opt, obj->receiver);
                }
                if (obj->message) {
                    Optimizer_Delete(opt, obj->message);
                }
            }

            Bytecode_Delete(opt->code, instr);
                // Delete the instruction itself.
        } else {
            // If the instruction is impure, don't delete it. Just indicate that
            // we don't care about its result.
            instr->result_type = RESULT_IGNORE;
        }

        obj->instr_offset = UNKNOWN_INSTR_OFFSET;
    }
}

SymbolicObject *Optimizer_Relocate(Optimizer *opt, SymbolicObject *obj)
{
    if (obj->instr_offset == GHOST_INSTR_OFFSET ||
        obj->instr_offset == UNKNOWN_INSTR_OFFSET)
    {
        // We can't relocate an object if we can't locate the instruction that
        // originally computed it.
        return NULL;
    }

    // Try to relocate the sub-expressions of `obj` before creating a new result
    // object, in case we fail to relocate the sub-expressions.
    const Instruction *instr = Bytecode_Get(opt->code, obj->instr_offset);
    if (instr->type == INSTR_BLOCKI || instr->type == INSTR_CLOSEI) {
        // Relocate captured objects, constructing a list of relocatable objects
        // in the same order as `obj->captures`.
        SymbolicObject *captures = NULL;
        SymbolicObject *captures_end = NULL;
        for (SymbolicObject *capture = obj->captures;
             capture != NULL;
             capture = capture->next)
        {
            SymbolicObject *reloc = Optimizer_Relocate(opt, capture);
            if (reloc == NULL) {
                return NULL;
            }

            if (captures_end == NULL) {
                captures = reloc;
            } else {
                captures_end->next = reloc;
            }
            captures_end = reloc;
        }

        // Create the result object.
        SymbolicObject *result;
        if (SymbolicObject_New(opt, &result) != BLIMP_OK) {
            return NULL;
        }
        result->value_type = VALUE_RELOCATABLE;
        result->instr_offset = obj->instr_offset;
        result->captures = captures;
        return result;
    } else {
        // Relocate the receiver and the message.
        SymbolicObject *receiver = NULL;
        if (obj->receiver != NULL) {
            receiver = Optimizer_Relocate(opt, obj->receiver);
            if (receiver == NULL) {
                return NULL;
            }
        }
        SymbolicObject *message = NULL;
        if (obj->message != NULL) {
            message = Optimizer_Relocate(opt, obj->message);
            if (message == NULL) {
                return NULL;
            }
        }

        // Create the result object.
        SymbolicObject *result;
        if (SymbolicObject_New(opt, &result) != BLIMP_OK) {
            return NULL;
        }
        result->value_type = VALUE_RELOCATABLE;
        result->instr_offset = obj->instr_offset;
        result->receiver = receiver;
        result->message = message;
        return result;
    }
}

Status Optimizer_EmitRelocatable(
    Optimizer *opt,
    SymbolicObject *obj,
    ResultType result_type,
    SymbolicObject **result)
{
    assert(obj->value_type == VALUE_RELOCATABLE);

    // Get the offset of the original instruction which generated this object,
    // and clear `obj->instr_offset`, since we are about to relocate this
    // instruction.
    size_t instr_offset = obj->instr_offset;
    obj->instr_offset = UNKNOWN_INSTR_OFFSET;

    // Relocatable objects must have valid instruction offets. This is ensured
    // by Optimizer_Relocate().
    assert(instr_offset != UNKNOWN_INSTR_OFFSET);
    assert(instr_offset != GHOST_INSTR_OFFSET);

    // Get the original instruction, and update its result type to match the
    // result type of the relocated code.
    Instruction *instr = Bytecode_Get(opt->code, instr_offset);
    instr->result_type = result_type;

    // Determine the result type of sub-expressions.
    switch (instr->type) {
        case INSTR_SYMI:
        case INSTR_BLOCKI:
        case INSTR_CLOSEI:
        case INSTR_OBJI:
        case INSTR_MSG:
        case INSTR_MSGOF:
            // This instruction is pure. We will only evaluate it if we are
            // actually interested in its result.
            if (Optimizer_UseResult(opt, instr->result_type)) {
                result_type = RESULT_USE;
            } else {
                // If we are not evaluating this instruction, we can ignore the
                // results of its sub-expressions and just compute them for
                // their side-effects.
                result_type = RESULT_IGNORE;
            }
            break;
        default:
            // This instruction might have side-effects, so we need to evaluate
            // it regardless of whether we are using its result. Therefore, we
            // do care about the results of its sub-expressions.
            result_type = RESULT_USE;
            break;
    }

    // Relocate instructions for the sub-expressions, and use the results to
    // construct a new object representing the result of this instruction.
    if (instr->type == INSTR_BLOCKI || instr->type == INSTR_CLOSEI) {
        // Relocate captured objects.
        SymbolicObject *captures = NULL;
        SymbolicObject *captures_end = NULL;
        for (SymbolicObject *capture = obj->captures;
             capture != NULL;
             capture = capture->next)
        {
            SymbolicObject *reloc;
            TRY(Optimizer_EmitRelocatable(
                opt, capture, result_type, &reloc));
            if (Optimizer_UseResult(opt, result_type)) {
                Optimizer_Pop(opt);
                    // The captures are going to be consumed by this BLOCKI or
                    // CLOSEI instruction, so they should not end up on the
                    // result stack.
                if (captures_end == NULL) {
                    captures = reloc;
                } else {
                    captures_end->next = reloc;
                }
                captures_end = reloc;
            }
        }

        instr = Bytecode_Get(opt->code, instr_offset);
        if (Optimizer_UseResult(opt, result_type)) {
            // Evaluate this instruction if necessary (either it has
            // side-effects, or we need its result).
            TRY(Emit(opt, instr, true, result));
            (*result)->captures = captures;
        } else {
            Bytecode_Delete(opt->code, instr);
        }
    } else {
        // Relocate the receiver and message, as appropriate.
        SymbolicObject *receiver = NULL;
        if (obj->receiver != NULL) {
            TRY(Optimizer_EmitRelocatable(
                opt, obj->receiver, result_type, &receiver));
            if (Optimizer_UseResult(opt, result_type)) {
                // The receiver is going to be consumed by this instruction, so
                // it should not end up on the result stack.
                Optimizer_Pop(opt);
            }
        }
        SymbolicObject *message = NULL;
        if (obj->message != NULL) {
            TRY(Optimizer_EmitRelocatable(
                opt, obj->message, result_type, &message));
            if (Optimizer_UseResult(opt, result_type)) {
                // The message is going to be consumed by this instruction, so
                // it should not end up on the result stack.
                Optimizer_Pop(opt);
            }
        }

        instr = Bytecode_Get(opt->code, instr_offset);
        if (Optimizer_UseResult(opt, result_type)) {
            // Evaluate this instruction if necessary (either it has
            // side-effects, or we need its result).
            TRY(Emit(opt, instr, true, result));
            (*result)->receiver = receiver;
            (*result)->message = message;
        } else {
            Bytecode_Delete(opt->code, instr);
        }
    }

    if (!Optimizer_UseResult(opt, result_type)) {
        // If we are not evaluating this instruction, generate a dummy object
        // representing a lack of knowledge about the result.
        TRY(SymbolicObject_New(opt, result));
    }

    obj->value_type = VALUE_UNKNOWN;
        // Clear the value type so that this object cannot be relocated twice.
    return BLIMP_OK;
}

SymbolicObject *Optimizer_GetMessage(Optimizer *opt, size_t index)
{
    return DBMap_Resolve(&opt->messages, index);
}

void Optimizer_ReplaceSubroutine(Optimizer *opt, Bytecode **to_replace)
{
    if (*to_replace == opt->replace_subroutine) {
        BlimpBytecode_Free(*to_replace);
        *to_replace = opt->optimized_subroutine;
        ++(*to_replace)->refcount;
    }
}

OptimizerCheckpoint Optimizer_SaveCheckpoint(Optimizer *opt)
{
    OptimizerCheckpoint checkpoint = {
        .code_offset = Bytecode_Offset(opt->code),
        .stack_size = SymbolicObjectStack_Size(&opt->result_stack),
        .ghost_objects = opt->ghost_objects,
    };
    CheckpointAllocator_Checkpoint(
        &opt->sym_objects, &checkpoint.sym_objects_checkpoint);
    return checkpoint;
}

void Optimizer_RestoreCheckpoint(
    Optimizer *opt, OptimizerCheckpoint checkpoint)
{
    Bytecode_Truncate(opt->code, checkpoint.code_offset);

    while (SymbolicObjectStack_Size(&opt->result_stack) >
           checkpoint.stack_size)
    {
        Optimizer_Pop(opt);
    }

    CheckpointAllocator_RestoreCheckpoint(
        &opt->sym_objects, &checkpoint.sym_objects_checkpoint);

    opt->ghost_objects = checkpoint.ghost_objects;
}

Status Optimizer_GhostCheck(
    Optimizer *opt, OptimizerCheckpoint checkpoint)
{
    if (opt->ghost_objects != checkpoint.ghost_objects) {
        return Error(Optimizer_Blimp(opt), BLIMP_ERROR);
    }

    return BLIMP_OK;
}
