#include "internal/blimp.h"
#include "internal/compile.h"
#include "internal/optimizer.h"

static Status SymEvalProcedure(
    Optimizer *opt,
    Bytecode *code,
    ScopedObject *scope,
    SymbolicObject **result);

// Is there enough static information about the value of `obj` to generate code
// which cmoputes an equivalent value?
static inline bool CanSymEvalObject(SymbolicObject *obj)
{
    switch (obj->value_type) {
        case VALUE_SYMBOL:
        case VALUE_OBJECT:
        case VALUE_RELOCATABLE:
            return true;
        case VALUE_LAMBDA:
            if (!(obj->value.lambda.flags & BLOCK_LAMBDA)) {
                // If the object is not a lambda (that is, it might use its
                // scope for internally mutable state) then a new object with
                // the same code is not equivalent; we would need to generate
                // code which produces the same reference, which is impossible.
                //
                // TODO This check is currently a bit too conservative because
                // the compiler is conservative about setting the BLOCK_LAMBDA
                // flag, because it doesn't have as much information as we do at
                // runtime. For example, if the compiler sees a send to a
                // symbol, it can never prove that that send does not use its
                // scope. At runtime, though, we know the send will not use the
                // scope as long as the symbol is already defined in a parent
                // scope. We can fix this by doing a runtime check to update the
                // BLOCK_LAMBDA flag if it is not set by the compiler. Ideally,
                // we'd want to save the result of this runtime check in the
                // generated code, not just in this SymbolicObject, so we can
                // take advantage of that information in future optimiztion
                // passes.
                return false;
            }

            // In order to symbolically evaluate a lambda, we need to
            // symbolically evaluate all of the objects which it captures.
            for (SymbolicObject *capture = obj->captures;
                 capture != NULL;
                 capture = capture->next)
            {
                if (!CanSymEvalObject(capture)) {
                    return false;
                }
            }

            return true;
        default:
            return false;
    }
}

// If CanSymEvalObject(obj), SymEvalObject() actually produces code which pushes
// a new object equivalent to `obj` onto the result stack.
static Status SymEvalObject(
    Optimizer *opt,
    SymbolicObject *obj,
    ResultType result_type,
    SymbolicObject **result)
{
    assert(CanSymEvalObject(obj));

    switch (obj->value_type) {
        case VALUE_SYMBOL: {
            SYMI new_instr = {
                {INSTR_SYMI, result_type, sizeof(SYMI)}, obj->value.symbol
            };
            TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, result));
            break;
        }

        case VALUE_LAMBDA: {
            SymbolicObject *captures = NULL;
            SymbolicObject *captures_end = NULL;
            size_t num_captures = 0;

            // Recursively evaluate objects captured by this object. The list of
            // captures is in order from innermost to outermost, so if we
            // evaluate them in order, the top of the result stack will end up
            // being the last object evaluated -- the outermost. This is the
            // order expected by the runtime interpreter.
            //
            // We want the new list of evaluated SymbolicObjects --
            // `captures` -- to be in in-to-out order, so each newly evaluated
            // object will be appended to the end of `captures` using
            // `captures_end`, which will track the last object in the list.
            for (SymbolicObject *capture = obj->captures;
                 capture != NULL;
                 capture = capture->next)
            {
                // Recursively evaluate the captured object.
                SymbolicObject *eval_capture;
                TRY(SymEvalObject(opt, capture, RESULT_USE, &eval_capture));
                Optimizer_Pop(opt);

                // Append the newly evaluated object to `captures`.
                if (captures_end == NULL) {
                    assert(captures == NULL);
                    captures = eval_capture;
                    captures_end = eval_capture;
                } else {
                    captures_end->next = eval_capture;
                    captures_end = eval_capture;
                }

                ++num_captures;
            }

            if (obj->value.lambda.scope == NULL) {
                BLOCKI new_instr = {
                    {INSTR_BLOCKI, result_type, sizeof(BLOCKI)},
                    obj->value.lambda.msg_name,
                    obj->value.lambda.code,
                    obj->value.lambda.flags,
                    num_captures,
                };
                ++new_instr.code->refcount;
                TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, result));
            } else {
                 CLOSEI new_instr = {
                    {INSTR_CLOSEI, result_type, sizeof(CLOSEI)},
                    ScopedObject_Borrow(obj->value.lambda.scope),
                    obj->value.lambda.msg_name,
                    obj->value.lambda.code,
                    obj->value.lambda.flags,
                    num_captures,
                };
                ++new_instr.code->refcount;
                TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, result));
            }

            (*result)->captures = captures;
            break;
        }

        case VALUE_OBJECT: {
            if (obj->value.object != NULL) {
                BlimpObject_Borrow(obj->value.object);
                assert(Object_Type(obj->value.object) != OBJ_SYMBOL);
            }

            OBJI new_instr = {
                {INSTR_OBJI, result_type, sizeof(OBJI)}, obj->value.object
            };
            TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, result));
            break;
        }

        case VALUE_RELOCATABLE:
            TRY(Optimizer_EmitRelocatable(opt, obj, result_type, result));
            break;

        default:
            assert(false);
    }

    SymbolicObject_CopyValue(obj, *result);
        // The entire point of this function is to compute a new object whose
        // value is equivaent to `obj`. So copy the static information about
        // the value from `obj` into the result.

    return BLIMP_OK;
}

// Symbolically execute a SENDTO or CALLTO instruction, producing equivalent
// code. This function performs optimizations such as constant elision if
// possible, using the fact that we know exactly which object will be receiving
// the message (since it's encoded in SENDTO and CALLTO instructions).
static Status SymEvalSendTo(
    Optimizer *opt,
    bool explicit_scope,
        // Is the scope of the send specified explicitly by the instruction?
        // `true` for CALLTO, `false` for SENDTO.
    Object *receiver,
    SymbolicObject *message,
    const SourceRange *range,
    SendFlags flags,
    ScopedObject *scope,
        // The scope in which the send will be evaluated, or an ancestor scope.
        // Used for constant elision if the receiver is a symbol.
    bool *ret,
        // Does this instruction result in a return?
    ResultType result_type,
    SymbolicObject **result)
{
    Blimp *blimp = Optimizer_Blimp(opt);

    *ret = !!(flags & SEND_TAIL);

    if (blimp->options.constant_elision) {
        // Try constant elision: while the receiver is a symbol whose value in
        // `scope` is constant, replace it with its value.
        Object *value;
        ScopedObject *owner;
        bool is_const;
        while (
            Object_Type(receiver) == OBJ_SYMBOL &&
            ScopedObject_Lookup(
                scope,
                (const Symbol *)receiver,
                &value,
                &owner,
                &is_const) &&
            is_const &&
            owner->seq <= opt->specialized
                // We can only do constant elision if the scope which owns the
                // symbol contains the scope in which we are specializing.
                // Otherwise, if the specialization scope contains the owning
                // scope, then this code might sometimes execute in the same
                // specialization scope but with a different owner scope, which
                // might have a different value for the symbol.
        ) {
            receiver = value;
        }
    }

    // If enabled, try unused message elision. If the message has not already
    // been optimized away (`message != NULL`) and if we can determine that the
    // receiver is a block which does not use its message, delete the `message`
    // and set it to `NULL`, indicating that it has been optimized away.
    //
    // We do this before trying inlining, because it may make inlining possible
    // when it previously wasn't, in the case where `message` was not a pure
    // value.
    if (blimp->options.unused_message_elision &&
        message != NULL &&
        Object_Type(receiver) == OBJ_BLOCK &&
        !(((BlockObject *)receiver)->flags & BLOCK_USES_MESSAGE))
    {
        Optimizer_Delete(opt, message);
        message = NULL;
    }

    // Try inlining if the receiver is a block object.
    if (blimp->options.inlining && Object_Type(receiver) == OBJ_BLOCK) {
        if (message != NULL &&
            !CanSymEvalObject(message) &&
                // We need to move the computation of the message to wherever it
                // is referenced in the inlined procedure. If the message is not
                // a pure value which can be recomputed as needed, we need to do
                // a relocation.
            (((BlockObject *)receiver)->flags & BLOCK_PURE) &&
                // We only relocate if the procedure being inlined is pure,
                // because if the message has side-effects, we cannot relocate
                // it after side-effects within the procedure, since this would
                // change the observable order of side-effects.
                //
                // Note that this check is a bit conservative; we could
                // theoretically relocate the message into an impure inline
                // procedure as long as all of the side-effects in the procedure
                // occur after it evalutes the message. But this adds
                // significantly more complexity, while the current check covers
                // most common cases.
            !(((BlockObject *)receiver)->flags & BLOCK_NOT_AFFINE)
                // We only inline an effectful message into a procedure which
                // might be affine (that is, is not definitely NOT_AFFINE),
                // since we can only evaluate an inlined effectful expression
                // once. "Might be affine" is sufficient because relocatable
                // objects have affine-ness built in, and inlining will fail if
                // we try to evaluate the relocated message more than once.
                // Thus, this check is merely an optimization so that we don't
                // going trying to inline non-affine procedures where inlining
                // will definitely fail.
        ) {
            SymbolicObject *reloc = Optimizer_Relocate(opt, message);
            if (reloc != NULL) {
                message = reloc;
            }
        }

        // We can inline if the message has been optimized away, or if it can be
        // evaluated inline (possibly after relocation above).
        if (message == NULL || CanSymEvalObject(message)) {
            // Create a symbolic stack frame to represent information about this
            // inline send, which will not be on the call stack at runtime since
            // we are inlining it.
            SymbolicFrame frame = {
                .flags      = BLOCK_DEFAULT,
                .tail_call  = flags & SEND_TAIL,
                .scope      = (ScopedObject *)receiver,
                .use_result = Optimizer_UseResult(opt, result_type),
                .arg        = message,
                .captures   = NULL,
            };
            if (opt->stack && !opt->stack->tail_call) {
                // Clear the tail_call flag if we're already inlining a call
                // which is not in a tail position in its parent procedure.
                frame.tail_call = false;
            }

            Optimizer_InlineCall(opt, &frame);
            Status status = SymEvalProcedure(
                opt,
                ((BlockObject *)receiver)->code,
                (ScopedObject *)receiver,
                result);
            Optimizer_InlineReturn(opt);

            if (status == BLIMP_OK) {
                if (message != NULL) Optimizer_Delete(opt, message);
                return BLIMP_OK;
            }
        }
    }

    // If we weren't able to inline the send, then we will generate an actual
    // send instruction. That will be either SENDTO or CALLTO, depending on
    // whether we need to specify the scope for the send explicitly in the
    // instruction. We need to specify the scope if...
    if (explicit_scope ||
            // ...we are executing a CALLTO instruction, which already specified
            // an explict scope, or...
        (opt->stack && Object_Type(receiver) == OBJ_SYMBOL &&
            // ...we are inlining, and sending a message to a symbol in a scope
            // which...
            (opt->stack->scope ||
                // ...is not the ambient scope, or...
            !ScopedObject_Lookup(
                scope, (const Symbol *)receiver, NULL, NULL, NULL)
                // ...does not already have a value for the symbol. In this
                // case, the symbol should be initialized in the scope
                // containing the procedure we're inlining, not in the scope
                // we're the inlined code will ultimately execute.
            )
        )
    ) {
        // We're going to emit a CALLTO instruction. Figure out which scope to
        // explicitly specify.
        ScopedObject *call_scope;
        if (explicit_scope) {
            // If there was an explicit scope in the original instruction, use
            // that.
            call_scope = scope;
        } else {
            // Otherwise, we want to use the scope of the object whose code is
            // being inlined.
            if (opt->stack->scope == NULL || opt->stack->closure) {
                // If we don't know the scope, we can't inline this procedure.
                return Error(blimp, BLIMP_ERROR);
            }
            call_scope = opt->stack->scope;
        }

        // Emit the CALLTO.
        CALLTO new_instr = {
            {INSTR_CALLTO, result_type, sizeof(CALLTO)},
            *range, ScopedObject_Borrow(call_scope),
            BlimpObject_Borrow(receiver), flags
        };
        if (opt->stack && !opt->stack->tail_call) {
            // Clear the tail call bit if the call being inlined is not in a
            // tail position in the overall procedure.
            new_instr.flags &= ~SEND_TAIL;
        }
        if (message == NULL) {
            // If the message has been optimized away (either just above, or by
            // the previous version of this instruction) indicate that by
            // setting the SEND_NO_MESSAGE flag.
            new_instr.flags |= SEND_NO_MESSAGE;
        }
        TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, result));
    } else {
        // In the case where the original instruction did not specify an
        // explicit scope and we are not initializing an out-of-scope symbol, we
        // just emit a SENDTO instruction with the (possibly updated via
        // constant elision) receiver.
        SENDTO new_instr = {
            {INSTR_SENDTO, result_type, sizeof(SENDTO)},
            *range, BlimpObject_Borrow(receiver), flags
        };
        if (opt->stack && !opt->stack->tail_call) {
            // Clear the tail call bit if the call being inlined is not in a
            // tail position in the overall procedure.
            new_instr.flags &= ~SEND_TAIL;
        }
        if (message == NULL) {
            // If the message has been optimized away (either just above, or by
            // the previous version of this instruction) indicate that by
            // setting the SEND_NO_MESSAGE flag.
            new_instr.flags |= SEND_NO_MESSAGE;
        }
        TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, result));
    }

    (*result)->message = message;

    return BLIMP_OK;
}

// Symbolically execute a SEND or CALL instruction, producing equivalent code.
// This function performs optimizations such as constant elision and inlining if
// possible.
static Status SymEvalSend(
    Optimizer *opt,
    bool explicit_scope,
        // Is the scope of the send specified explicitly by the instruction?
        // `true` for CALL, `false` for SEND.
    SymbolicObject *receiver,
    SymbolicObject *message,
    const SourceRange *range,
    SendFlags flags,
    ScopedObject *scope,
        // The scope in which the send will be evaluated, or an ancestor scope.
        // Used for constant elision if the receiver is a symbol.
    bool *ret,
        // Does this instruction result in a return?
    ResultType result_type,
    SymbolicObject **result)
{
    Blimp *blimp = Optimizer_Blimp(opt);

    *ret = !!(flags & SEND_TAIL);

    // See if we statically know something about the value of the receiver which
    // we can use to generate more efficient code.
    switch (receiver->value_type) {
        case VALUE_SYMBOL: {
            // If the receiver is a symbol immediate, we can turn this SEND or
            // CALL into a SENDTO or CALLTO, since we know exactly what the
            // receiving object is. It will always be more efficient to optimize
            // a SENDTO or CALLTO since the instruction encodes stricly more
            // information, so just delegate to SymEvalSendTo().
            TRY(SymEvalSendTo(
                opt,
                explicit_scope,
                (Object *)receiver->value.symbol,
                message,
                range,
                flags,
                scope,
                ret,
                result_type,
                result));

            Optimizer_Delete(opt, receiver);
            return BLIMP_OK;
        }

        case VALUE_OBJECT: {
            // Similar to the VALUE_SYMBOL case, if the receiver is a specific
            // Object, we can turn the instruction into a SENDTO or CALLTO and
            // delegate to SymEvalSendTo().
            TRY(SymEvalSendTo(
                opt,
                explicit_scope,
                receiver->value.object,
                message,
                range,
                flags,
                scope,
                ret,
                result_type,
                result));

            Optimizer_Delete(opt, receiver);
            return BLIMP_OK;
        }

        case VALUE_LAMBDA: {
            // If the receiver is an anonymous block, as in
            //          `{^param lambda_body(^param)} message`
            // we can try to inline the receiver's code, dispensing with the
            // need to create a temporary block object to represent the receiver
            // and possibly producing more optimized code by evaluating the
            // message in the context of the receiver's code:
            //          `lambda_body(message)`

            // If enabled, try unused message elision. If the message has not
            // already been optimized away (`message != NULL`) and if we can
            // determine that the receiver is a block which does not use its
            // message, delete the `message` and set it to `NULL`, indicating
            // that it has been optimized away.
            //
            // We do this before trying inlining, because it may make inlining
            // possible when it previously wasn't, in the case where `message`
            // was not a pure value.
            if (blimp->options.unused_message_elision &&
                message != NULL &&
                !(receiver->value.lambda.flags & BLOCK_USES_MESSAGE))
            {
                Optimizer_Delete(opt, message);
                message = NULL;
            }

            if (!blimp->options.inlining) {
                // If inlining is disabled, we cannot proceed with the inlining
                // optimization.
                break;
            }

            if (message != NULL && !CanSymEvalObject(message)) {
                // If the message is not a pure value, we can't substitute the
                // message into the lambda's body, because that might cause the
                // message's side-effects to be evaluated in the wrong order or
                // more than once.
                //
                // However, if the receiver is pure, we can try relocating the
                // computation of the message into the inline procedure where it
                // references its message. We only do this if the receiver is
                // pure, because otherwise we might relocate the effectful
                // computation of the message after some side-effects performed
                // by the inlined procedure, thus reversing the order of side-
                // effects.
                //
                // Note that this is a bit conservative; we could theoretically
                // relocate the message into an impure inline procedure as long
                // as all of the side-effects in the procedure occur after it
                // evalutes the message. But this adds significantly more
                // complexity, while the current check covers most common cases.
                if (!(receiver->value.lambda.flags & BLOCK_PURE)) {
                    break;
                }

                if (receiver->value.lambda.flags & BLOCK_NOT_AFFINE) {
                    // We only relocate an effectful message into a procedure
                    // which might be affine (that is, is not definitely
                    // NOT_AFFINE), since we can only evaluate an inlined
                    // effectful expression once. "Might be affine" is
                    // sufficient because relocatable objects have affine-ness
                    // built in, and inlining will fail if we try to evaluate
                    // the relocated message more than once. Thus, this check is
                    // merely an optimization so that we don't going trying to
                    // inline non-affine procedures where inlining will
                    // definitely fail.
                    break;
                }

                SymbolicObject *reloc = Optimizer_Relocate(opt, message);
                if (reloc == NULL) {
                    break;
                }
                message = reloc;
            }

            // Now that we're going to be processing an inline procedure, we
            // need to create a symbolic stack frame to represent information
            // about this particular send, which will not be on the call stack
            // at runtime since we are eliminating the send.
            SymbolicFrame frame = {
                .flags      = receiver->value.lambda.flags,
                .tail_call  = flags & SEND_TAIL,
                .scope      = receiver->value.lambda.scope,
                .closure    = true,
                .use_result = Optimizer_UseResult(opt, result_type),
                .arg        = message,
                .captures   = receiver->captures,
            };
            if (opt->stack && !opt->stack->tail_call) {
                // Clear the tail_call flag if we're already inlining a call
                // which is not in a tail position in its parent procedure.
                frame.tail_call = false;
            }

            Optimizer_InlineCall(opt, &frame);
            Status status = SymEvalProcedure(
                opt,
                receiver->value.lambda.code,
                scope,
                    // We evaluate the inlined lambda in the same scope we're
                    // currently in. This would be the parent of the block's
                    // actual scope, but since the lambda will not create any
                    // new symbols in its scope, all symbol lookups that it does
                    // will be resolved in its parent or one of its ancestors.
                    // Therefore, we can skip creating a scope for the lambda
                    // and just use its parent's scope.
                    //
                    // This property of the lambda (that it may not define any
                    // symbols which are not already in scope) will be enforced
                    // by SymEvalProcedure since we are giving it an inline
                    // frame with a `NULL` scope: if it detects that the
                    // procedure actually will (or might) try to define a symbol
                    // which is not already in scope, it will return an error,
                    // and we will bail out of this optimization.
                result
            );
            Optimizer_InlineReturn(opt);

            if (status != BLIMP_OK) {
                // We might fail to evaluate the procedure inline for a number
                // of reasons which should not be fatal to the overall
                // execution. For example, if the procedure might initialize a
                // symbol in the scope which we've elided, SymEvalProcedure()
                // will return an error, as discussed above. Or, if the
                // procedure references a captured message which is no longer
                // captured because the object which would have captured it has
                // been inlined, and if the would-be value of that captured
                // message is not known statically, then SymEvalProcedure() may
                // return an error.
                //
                // These kinds of errors should not be propagated to the caller.
                // Instead, we just give up on the inlining optimization and
                // translate the send normally.
                break;
            }

            // We have now successfully inlined the send, and we're not going to
            // emit a new SEND or CALL instruction. So we don't need the
            // receiver and message to ever be pushed onto the result stack in
            // the first place.
            Optimizer_Delete(opt, receiver);
            if (message != NULL) Optimizer_Delete(opt, message);

            return BLIMP_OK;
        }

        default:
            break;
    }

    // If we could have optimized this send, we would have done so and returned
    // from the switch above. Now, we fall back to simply translating it and
    // emitting a new SEND or CALL instruction.
    if (explicit_scope || opt->stack) {
        // If we're specifying the scope explicitly, we need to emit a CALL
        // instruction, not a SEND.
        //
        // Or, if we're inlining, we need to specify the scope explicitly,
        // because we don't know the type of the receiver, and in particular, we
        // don't know whether it is a symbol which is not yet in scope; it might
        // be. Thus, since the procedure might make use of the receiver's scope,
        // but the receiver is not on the call stack since it's inlined, we must
        // explicitly specify the scope.

        // Figure out which scope to explicitly specify.
        ScopedObject *call_scope;
        if (explicit_scope) {
            // If there was an explicit scope in the original instruction, use
            // that.
            call_scope = scope;
        } else {
            // Otherwise, we want to use the scope of the object whose code is
            // being inlined.
            if (opt->stack->scope == NULL) {
                // If we don't know the scope (perhaps the scope object was
                // optimized away entirely, as we do above when inlining a
                // lambda), then we can't inline this procedure.
                return Error(blimp, BLIMP_ERROR);
            }
            call_scope = opt->stack->scope;
        }

        // Emit the CALL.
        CALL new_instr = {
            {INSTR_CALL, result_type, sizeof(CALL)},
            *range, ScopedObject_Borrow(call_scope), flags
        };
        if (opt->stack && !opt->stack->tail_call) {
            // Clear the tail call bit if the call being inlined is not
            // in a tail position in the overall procedure.
            new_instr.flags &= ~SEND_TAIL;
        }
        if (message == NULL) {
            // If the message has been optimized away (either just above, or by
            // the previous version of this instruction) indicate that by
            // setting the SEND_NO_MESSAGE flag.
            new_instr.flags |= SEND_NO_MESSAGE;
        }
        TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, result));
    } else {
        // In the normal case, there is no explicit scope encoded in the
        // original instruction, and the scope which should be affected if the
        // receiver turns out to be a symbol corresponds with the scope that
        // will be on the call stack at runtime, since we're not inlining.
        // Therefore, we can emit a SEND.
        SEND new_instr = {
            {INSTR_SEND, result_type, sizeof(SEND)}, *range, flags
        };
        if (opt->stack && !opt->stack->tail_call) {
            new_instr.flags &= ~SEND_TAIL;
        }
        if (message == NULL) {
            // If the message has been optimized away (either just above, or by
            // the previous version of this instruction) indicate that by
            // setting the SEND_NO_MESSAGE flag.
            new_instr.flags |= SEND_NO_MESSAGE;
        }
        TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, result));
    }

    (*result)->receiver = receiver;
    (*result)->message = message;

    return BLIMP_OK;
}

static Status SymEvalInstructionAndPushResult(
    Optimizer *opt,
    const Instruction *ip,
    ScopedObject *scope,
    bool *ret,
    SymbolicObject **result)
{
    Blimp *blimp = Optimizer_Blimp(opt);

    *ret = false;

    switch (ip->type) {
        case INSTR_SYMI: {
            SYMI *instr = (SYMI *)ip;

            // No special optimizations to do for symbol literals; just emit a
            // copy of the instruction.
            TRY(Optimizer_Emit(opt, ip, result));

            // We do know something about the result though.
            (*result)->value_type = VALUE_SYMBOL;
            (*result)->value.symbol = instr->sym;

            return BLIMP_OK;
        }

        case INSTR_OBJI: {
            OBJI *instr = (OBJI *)ip;

            assert(
                instr->object == NULL ||
                Object_Type(instr->object) != OBJ_SYMBOL);

            // We're going to emit a copy of the instruction. Since each copy of
            // the instruction owns a reference to the object, we need to borrow
            // the object once more.
            if (instr->object != NULL) {
                BlimpObject_Borrow(instr->object);
            }
            TRY(Optimizer_Emit(opt, ip, result));

            // We know exactly what the result of this instruction will be.
            (*result)->value_type = VALUE_OBJECT;
            (*result)->value.object = instr->object;

            return BLIMP_OK;
        }

        case INSTR_BLOCKI: {
            BLOCKI *instr = (BLOCKI *)ip;

            // Pop captured messages from the stack. The messages are ordered on
            // the stack so that the outermost message is on top. We want the
            // list of captures to be in the opposite order (innermost to
            // outermost) so we add objects to the front of the list as we pop
            // them off the stack. Later on, though, we will need to append to
            // the end of this list, so we maintain a pointer `captures_end` to
            // the last object in the list.
            SymbolicObject *captures = NULL;
            SymbolicObject *captures_end = NULL;
            for (size_t i = 0; i < instr->captures; ++i) {
                SymbolicObject *capture = Optimizer_Pop(opt);
                assert(capture->next == NULL);
                capture->next = captures;
                captures = capture;
                if (captures_end == NULL) {
                    captures_end = capture;
                }
            }

            // Now, if we're inlining, there are one or more objects between the
            // object we're creating and the object which will be its parent. We
            // need to capture the argument to the innermost inlined scope, as
            // well as any messages captured by that scope. These objects are
            // represented on the inline stack.
            size_t num_captures = 0;
            if (opt->stack) {
                // The innermost object we need to capture is the argument to
                // the current inlined procedure.
                SymbolicObject *capture;
                if (instr->flags & BLOCK_CLOSURE) {
                    // Only capture it if the compiler flags suggest it is going
                    // to be used.
                    if (!CanSymEvalObject(opt->stack->arg)) {
                        return Error(blimp, BLIMP_ERROR);
                    }
                    TRY(SymEvalObject(opt, opt->stack->arg, RESULT_USE, &capture));
                } else {
                    // If the compiler has determined that the new object should
                    // not be closed over its parent message (and hence the
                    // BLOCK_CLOSURE flag is not set) then we don't need to
                    // capture the actual object here. But we do still need to
                    // push _something_ on the stack as a placeholder, otherwise
                    // all subsequent captures would be shifted by 1 and have
                    // the wrong index. So push a NULL object onto the stack.
                    OBJI push_null = {
                        {INSTR_OBJI, RESULT_USE, sizeof(OBJI)}, NULL
                    };
                    TRY(Optimizer_Emit(
                        opt, (Instruction *)&push_null, &capture));
                    capture->value_type = VALUE_OBJECT;
                    capture->value.object = NULL;
                }
                // Get the object which we just evaluated from the result stack
                // and append it to the list of captures.
                capture = Optimizer_Pop(opt);
                assert(capture->next == NULL);
                if (captures_end == NULL) {
                    assert(captures == NULL);
                    captures = capture;
                } else {
                    captures_end->next = capture;
                }
                captures_end = capture;
                ++num_captures;

                // Now capture any objects which are also captured by the
                // current inlined scope.
                for (capture = opt->stack->captures;
                     capture != NULL;
                     capture = capture->next)
                {
                    SymbolicObject *obj;
                    if (!CanSymEvalObject(capture)) {
                        return Error(blimp, BLIMP_ERROR);
                    }
                    TRY(SymEvalObject(opt, capture, RESULT_USE, &obj));
                    Optimizer_Pop(opt);
                    ++num_captures;

                    assert(obj->next == NULL);
                    assert(captures_end != NULL);
                    captures_end->next = obj;
                    captures_end = obj;
                }
            }

            // Determine whether the new object's parent should be the scope
            // from the call stack at runtime (`parent == NULL`, in which case
            // we will emit a BLOCKI instruction) or whether it should be a
            // specific, statically-known scope (in which case we will emit a
            // SCOPEI instruction).
            //
            // In the former case, we need to tell the runtime interpreter
            // whether it should capture the message currently being processed
            // by the parent when it reads the parent from the call stack. This
            // is determined by the `flags` field of the outermost inline
            // object; that is, the object closest to the parent object in the
            // lexical hierarchy. That is this object itself, if there are no
            // scopes on the inline stack. Otherwise, we can get the `flags`
            // field from the inline stack frame.
            ScopedObject *parent = NULL;
            BlockFlags flags = instr->flags;
            if (opt->stack) {
                parent = opt->stack->scope;

                // Keep all of the flags except BLOCK_CLOSURE the same. The new
                // BLOCK_CLOSURE flag comes from the stack to reflect that the
                // scope in which the new object is a closure is not its
                // immediate parent (which is inlined) but rather the ultimate
                // parent of whatever is currently on the inline stack.
                flags &= ~BLOCK_CLOSURE;
                flags |= (opt->stack->flags & BLOCK_CLOSURE);
            }

            // Replace the new block's code with a more optimized version if
            // one is available.
            Bytecode *code = instr->code;
            ++code->refcount;
                // We take a new reference to the code because it's going to
                // be owned by the instruction we emit.
            Optimizer_ReplaceSubroutine(opt, &code);

            if (parent == NULL) {
                // If the parent of the new object is supposed to be the scope
                // from the runtime call stack, emit a BLOCKI instruction.
                BLOCKI new_instr = {
                    {INSTR_BLOCKI, ip->result_type, sizeof(BLOCKI)},
                    instr->msg_name, code, flags,
                    instr->captures + num_captures
                };
                TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, result));
            } else {
                // Otherwise, use the statically-known parent object to emit a
                // CLOSEI instruction.
                CLOSEI new_instr = {
                    {INSTR_CLOSEI, ip->result_type, sizeof(CLOSEI)},
                    ScopedObject_Borrow(parent), instr->msg_name, code,
                    flags, instr->captures + num_captures
                };
                TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, result));
            }

            // We have some static information about the value of this BLOCKI or
            // CLOSEI instruction (for example, we know it's captures and we
            // know what bytecode procedure it references). This can be useful
            // later on for potentially inlining sends to this object.
            (*result)->captures = captures;
            (*result)->value_type = VALUE_LAMBDA;
            (*result)->value.lambda.scope = parent;
            (*result)->value.lambda.msg_name = instr->msg_name;
            (*result)->value.lambda.code = instr->code;
            (*result)->value.lambda.flags = flags;

            return BLIMP_OK;
        }

        case INSTR_CLOSEI: {
            CLOSEI *instr = (CLOSEI *)ip;
            CLOSEI new_instr = *instr;

            // Pop captured messages from the result stack.
            SymbolicObject *captures = NULL;
            for (size_t i = 0; i < instr->captures; ++i) {
                SymbolicObject *capture = Optimizer_Pop(opt);
                assert(capture->next == NULL);
                capture->next = captures;
                captures = capture;
            }

            // Replace the bytecode procedure with an optimized version if one
            // is available.
            ++instr->code->refcount;
            Optimizer_ReplaceSubroutine(opt, &new_instr.code);

            // Emit the new instruction.
            ScopedObject_Borrow(instr->scope);
                // We need to borrow the scope object since both the old and new
                // instruction will now reference it.
            TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, result));

            // Fill in information about the result.
            (*result)->captures = captures;
            (*result)->value_type = VALUE_LAMBDA;
            (*result)->value.lambda.scope = instr->scope;
            (*result)->value.lambda.msg_name = instr->msg_name;
            (*result)->value.lambda.code = instr->code;
            (*result)->value.lambda.flags = instr->flags;

            return BLIMP_OK;
        }

        case INSTR_MSG: {
            MSG *instr = (MSG *)ip;
            SymbolicObject *obj = NULL;
            size_t index = instr->index;

            // Messages relative to the current scope can be represented in
            // several places. The innermost message (index 0) is the argument
            // to the current procedure, which is either represented in
            // `opt->stack->arg` if we are inlining or on the call stack at
            // runtime if we're not inlining. Subsequent messages may be
            // captured by the inline stack frame, if there is one. And then the
            // outermost messages are captured by either the statically-known
            // scope object in which we are inlining, or the scope which will be
            // represented on the runtime stack if we are not inlining.
            //
            // We will attempt to create a symbolic representation of the object
            // corresponding to `instr->index` by searching all of these
            // possible locations from innermost to outermost, updating `index`
            // as we go. Once we find a symbolic representation, we will try a
            // variety of strategies to evaluate it, such as recomputing an
            // equivalent value if possible using SymEvalObject, or emitting a
            // MSG or MSGOF instruction to look up the object at runtime if
            // SymEvalObject fails.
            //
            // We begin by looking at the argument to the procedure currently
            // being inlined, if there is one.
            if (opt->stack) {
                if (index == 0) {
                    // If `index` is 0, we've found our object.
                    obj = opt->stack->arg;
                } else {
                    // Otherwise, decrement `index` to account for the fact that
                    // we are skipping this capture.
                    --index;

                    // Next, we'll search through the objects captured by this
                    // stack frame. This list is already in the correct,
                    // in-to-out order.
                    for (SymbolicObject *capture = opt->stack->captures;
                         capture != NULL;
                         capture = capture->next)
                    {
                        if (index == 0) {
                            obj = capture;
                            break;
                        }
                        --index;
                    }
                }
            }

            if (obj != NULL) {
                // If we found our object in the search of the inline frame,
                // then due to inlining this object is not captured by the scope
                // in which this code will execute, so we cannot compute it
                // using a MSG or MSGOF instruction. Instead, we only have two
                // choices: either we generate code which computes a new object
                // with an equivalent value...
                if (CanSymEvalObject(obj)) {
                    return SymEvalObject(opt, obj, ip->result_type, result);
                }

                // ...or we emit a ghost object, with no code to generate it at
                // runtime, and hope that downstream optimizations are able to
                // eliminate the ghost object completely.
                TRY(Optimizer_EmitGhost(opt, ip->result_type, result));
                SymbolicObject_Copy(opt, obj, *result);
                return BLIMP_OK;
            }

            // If we didn't find the object on the inline stack frame above,
            // then it is captured by our parent scope. Our parent is either
            // stored on the inline stack, or it is the ambient scope object
            // which will be stored in the call stack at runtime.
            ScopedObject *parent = NULL;
            if (opt->stack) {
                parent = opt->stack->scope;
            }

            if (parent == NULL) {
                // If the parent scope is the ambient scope, get a symbolic
                // representation of the `index`th ambient message from the
                // optimizer state.
                obj = Optimizer_GetMessage(opt, index);
            } else {
                // Otherwise, the parent is statically known, so we can look up
                // it's `index`th captured message directly.
                Ref *ref = DBMap_Resolve(&parent->captures, index);
                assert(ref != NULL);
                assert(ref->to != NULL);
                TRY(Optimizer_SymbolizeObject(opt, ref->to, &obj));
            }

            // Try various strategies to evaluate `obj`.
            if (CanSymEvalObject(obj)) {
                // If we know the value of `obj`, just ignore the whole message
                // index business altogether and generate code which produces
                // and equivalent value.
                return SymEvalObject(opt, obj, ip->result_type, result);
            } else if (parent != NULL) {
                // If we have a scope which we know captures the message we're
                // looking for, emit a MSGOF instruction to retrieve the message
                // from that scope.
                MSGOF new_instr = {
                    {INSTR_MSGOF, ip->result_type, sizeof(MSGOF)},
                    ScopedObject_Borrow(parent), index
                };
                TRY(Optimizer_Emit(
                    opt, (Instruction *)&new_instr, result));
            } else {
                // Otherwise, the desired object is captured by the scope on the
                // runtime call stack.
                MSG new_instr = {
                    {INSTR_MSG, ip->result_type, sizeof(MSG)}, index
                };
                TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, result));
            }

            SymbolicObject_Copy(opt, obj, *result);
                // Whatever method we used to evaluate the object, the new
                // object has the same value as the old object.
            return BLIMP_OK;
        }

        case INSTR_MSGOF: {
            MSGOF *instr = (MSGOF *)ip;

            // Emit a copy of the MSGOF instruction.
            ScopedObject_Borrow(instr->scope);
            TRY(Optimizer_Emit(opt, ip, result));

            // We should be able to statically determine which captured object
            // is represented by this instruction, since we have access to the
            // capturing object. Record that information with the result.
            Ref *ref = DBMap_Resolve(&instr->scope->captures, instr->index);
            if (ref != NULL) {
                if (Object_Type(ref->to) == OBJ_SYMBOL) {
                    (*result)->value_type = VALUE_SYMBOL;
                    (*result)->value.symbol = (const Symbol *)ref->to;
                } else {
                    (*result)->value_type = VALUE_OBJECT;
                    (*result)->value.object = ref->to;
                }
            }

            return BLIMP_OK;
        }

        case INSTR_SEND: {
            SEND *instr = (SEND *)ip;

            SymbolicObject *message = NULL;
            if (!(instr->flags & SEND_NO_MESSAGE)) {
                message = Optimizer_Pop(opt);
            }
            SymbolicObject *receiver = Optimizer_Pop(opt);

            return SymEvalSend(
                opt,
                false,
                receiver,
                message,
                &instr->range,
                instr->flags,
                scope,
                ret,
                ip->result_type,
                result);
        }

        case INSTR_CALL: {
            CALL *instr = (CALL *)ip;

            SymbolicObject *message = NULL;
            if (!(instr->flags & SEND_NO_MESSAGE)) {
                message = Optimizer_Pop(opt);
            }
            SymbolicObject *receiver = Optimizer_Pop(opt);

            return SymEvalSend(
                opt,
                true,
                receiver,
                message,
                &instr->range,
                instr->flags,
                instr->scope,
                ret,
                ip->result_type,
                result);
        }

        case INSTR_SENDTO: {
            SENDTO *instr = (SENDTO *)ip;

            SymbolicObject *message = NULL;
            if (!(instr->flags & SEND_NO_MESSAGE)) {
                message = Optimizer_Pop(opt);
            }

            return SymEvalSendTo(
                opt,
                false,
                instr->receiver,
                message,
                &instr->range,
                instr->flags,
                scope,
                ret,
                ip->result_type,
                result);
        }

        case INSTR_CALLTO: {
            CALLTO *instr = (CALLTO *)ip;

            SymbolicObject *message = NULL;
            if (!(instr->flags & SEND_NO_MESSAGE)) {
                message = Optimizer_Pop(opt);
            }

            return SymEvalSendTo(
                opt,
                true,
                instr->receiver,
                message,
                &instr->range,
                instr->flags,
                instr->scope,
                ret,
                ip->result_type,
                result);
        }

        case INSTR_MACRO: {
            Optimizer_Pop(opt); // Handler
            Optimizer_Pop(opt); // Production
            return Optimizer_Emit(opt, ip, result);
        }

        case INSTR_RET: {
            *ret = true;

            if (opt->stack && !opt->stack->tail_call) {
                // If we encounter a RET in a procedure being inlined, don't
                // actually emit a RET, since that would return from the parent
                // procedure that we're inlined into.
                return BLIMP_OK;
            }

            if (blimp->options.tail_call_elimination) {
                // If we are returning and the previous instruction is a send or
                // call, make it a tail call instead of emitting a RET.
                Instruction *prev = Optimizer_LastInstruction(opt);
                assert(prev->result_type == RESULT_INHERIT);
                    // If `prev` is the last instruction before a RET, it's
                    // result type should always be INHERIT.
                SendFlags *flags = NULL;
                switch (prev->type) {
                    case INSTR_SEND:
                        flags = &((SEND *)prev)->flags;
                        break;
                    case INSTR_SENDTO:
                        flags = &((SENDTO *)prev)->flags;
                        break;
                    case INSTR_CALL:
                        flags = &((CALL *)prev)->flags;
                        break;
                    case INSTR_CALLTO:
                        flags = &((CALLTO *)prev)->flags;
                        break;
                    default:
                        break;
                }
                if (flags != NULL) {
                    assert(!(*flags & SEND_TAIL));
                        // The instruction should not already be a tail call, if
                        // there is a RET after it.
                    *flags |= SEND_TAIL;
                    return BLIMP_OK;
                }
            }

            return Optimizer_Emit(opt, ip, result);
        }

        case INSTR_NOP: {
            return Optimizer_Emit(opt, ip, result);
        }
    }

    assert(false);
    return Error(blimp, BLIMP_ERROR);
}

static Status SymEvalInstruction(
    Optimizer *opt,
    const Instruction *ip,
    ScopedObject *scope,
    bool *ret,
    SymbolicObject **result_ptr)
{
    SymbolicObject *result = NULL;
    TRY(SymEvalInstructionAndPushResult(opt, ip, scope, ret, &result));
    if (result != NULL) {
        *result_ptr = result;
        if (!Optimizer_UseResult(opt, ip->result_type)) {
            // If the result object is unused, see if we can delete the
            // instructions which generate it.
            Optimizer_Delete(opt, result);
        }
    }
    return BLIMP_OK;
}

static Status SymEvalProcedure(
    Optimizer *opt,
    Bytecode *code,
    ScopedObject *scope,
    SymbolicObject **result)
{
    Blimp *blimp = Optimizer_Blimp(opt);
    OptimizerCheckpoint checkpoint = Optimizer_SaveCheckpoint(opt);

    // Evaluate instructions sequentially until we reach an instruction which
    // causes a return, or the end of the procedure.
    for (const Instruction *ip = Bytecode_Begin(code);
         ip != Bytecode_End(code);
         ip = Instruction_Next(ip))
    {
        bool ret;
        if (SymEvalInstruction(opt, ip, scope, &ret, result) != BLIMP_OK) {
            Optimizer_RestoreCheckpoint(opt, checkpoint);
            return Reraise(blimp);
        }

        if (ret) {
            break;
        }
    }

    // At the end of a evaluating a procedure, all ghost objects created during
    // the procedure should have been optimized a way. If not, we need to raise
    // an error so we can roll back whatever upstream optimization caused the
    // ghost objects to be created.
    if (Optimizer_GhostCheck(opt, checkpoint) != BLIMP_OK) {
        Optimizer_RestoreCheckpoint(opt, checkpoint);
        return Reraise(blimp);
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
    Blimp *blimp = Object_Blimp((Object *)scope);
    Optimizer *opt = &blimp->optimizer;

    TRY(Optimizer_Begin(
        opt,
        scope,
        0,
        Bytecode_Expr(code),
        replace_subroutine,
        optimized_subroutine,
        specialized));
    SymbolicObject *result;
    Status status = SymEvalProcedure(opt, code, scope, &result);
    Optimizer_Pop(opt);
        // Clean up the final result from the result stack.
    Optimizer_End(opt, optimized);

    return status;
}

Status Optimize(
    Bytecode *code, ScopedObject *scope, size_t depth, Bytecode **optimized)
{
    Blimp *blimp = Object_Blimp((Object *)scope);
    Optimizer *opt = &blimp->optimizer;

    TRY(Optimizer_Begin(
        opt,
        scope,
        depth,
        Bytecode_Expr(code),
        NULL, NULL, 0));
    SymbolicObject *result;
    Status status = SymEvalProcedure(opt, code, scope, &result);
    Optimizer_Pop(opt);
        // Clean up the final result from the result stack.
    Optimizer_End(opt, optimized);

    return status;
}
