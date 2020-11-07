#include "internal/blimp.h"
#include "internal/compile.h"
#include "internal/optimizer.h"

static Status SymEvalProcedure(
    Optimizer *opt, Bytecode *code, ScopedObject *scope);

// Is there enough static information about the value of `obj` to generate code
// which cmoputes an equivalent value?
static inline bool CanSymEvalObject(SymbolicObject *obj)
{
    switch (obj->value_type) {
        case VALUE_SYMBOL:
        case VALUE_OBJECT:
            return true;
        case VALUE_LAMBDA:
            return obj->value.lambda.captures == NULL;
                // For simplicity, we don't treat BLOCKI or SCOPEI instructions
                // which capture arguments as values, because to compute an
                // equivalent value, we'd have to recursively check if we can
                // compute equivalent values for the captured arguments, which
                // gets extremely messy.
        default:
            return false;
    }
}

// If CanSymEvalObject(obj), SymEvalObject() actually produces code which pushes
// a new object equivalent to `obj` onto the result stack.
static inline Status SymEvalObject(
    Optimizer *opt, SymbolicObject *obj, ResultType result_type)
{
    assert(CanSymEvalObject(obj));

    SymbolicObject *result;
    switch (obj->value_type) {
        case VALUE_SYMBOL: {
            SYMI new_instr = {
                {INSTR_SYMI, result_type, sizeof(SYMI)}, obj->value.symbol
            };
            TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, &result));
            break;
        }

        case VALUE_LAMBDA: {
            assert(obj->value.lambda.captures == NULL);

            if (obj->value.lambda.scope == NULL) {
                BLOCKI new_instr = {
                    {INSTR_BLOCKI, result_type, sizeof(BLOCKI)},
                    obj->value.lambda.msg_name,
                    obj->value.lambda.code,
                    obj->value.lambda.flags,
                    obj->value.lambda.specialized,
                    0,
                };
                ++new_instr.code->refcount;
                TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, &result));
            } else {
                 CLOSEI new_instr = {
                    {INSTR_CLOSEI, result_type, sizeof(CLOSEI)},
                    ScopedObject_Borrow(obj->value.lambda.scope),
                    obj->value.lambda.msg_name,
                    obj->value.lambda.code,
                    obj->value.lambda.flags,
                    obj->value.lambda.specialized,
                    0,
                };
                ++new_instr.code->refcount;
                TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, &result));
            }
            break;
        }

        case VALUE_OBJECT: {
            OBJI new_instr = {
                {INSTR_OBJI, result_type, sizeof(OBJI)}, obj->value.object
            };
            TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, &result));
            break;
        }

        default:
            assert(false);
    }

    SymbolicObject_CopyValue(obj, result);
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
    ResultType result_type)
{
    Blimp *blimp = Optimizer_Blimp(opt);

    *ret = !!(flags & SEND_TAIL);

    SymbolicObject *result;
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

    // We need to specify the scope for the send explicitly if...
    if (explicit_scope ||
            // ...we are executing a CALLTO instruction, which already specified
            // an explict scope, or...
        (opt->stack && Object_Type(receiver) == OBJ_SYMBOL &&
         !ScopedObject_Lookup(
            scope, (const Symbol *)receiver, NULL, NULL, NULL)
        )
            // ... we are inlining, and we are sending a message to a symbol
            // which does not exist in `scope`; that is, we are initializing the
            // symbol. In this case, the symbol should be initialized in the
            // scope containing the procedure we're inlining, not in the scope
            // we're the inlined code will ultimately execute.
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
            if (opt->stack->scope == NULL) {
                // If we don't know the scope, we can't inline this procedure.
                return Error(blimp, BLIMP_ERROR);
            }
            call_scope = opt->stack->scope;
        }

        // Emit the CALLTO.
        CALLTO new_instr = {
            {INSTR_CALLTO, result_type, sizeof(SENDTO)},
            *range, flags, ScopedObject_Borrow(call_scope),
            BlimpObject_Borrow(receiver)
        };
        if (opt->stack && !opt->stack->tail_call) {
            // Clear the tail call bit if the call being inlined is not in a
            // tail position in the overall procedure.
            new_instr.flags &= ~SEND_TAIL;
        }
        TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, &result));
    } else {
        // In the case where the original instruction did not specify an
        // explicit scope and we are not initializing an out-of-scope symbol, we
        // just emit a SENDTO instruction with the (possibly updated via
        // constant elision) receiver.
        SENDTO new_instr = {
            {INSTR_SENDTO, result_type, sizeof(SENDTO)},
            *range, flags, BlimpObject_Borrow(receiver)
        };
        if (opt->stack && !opt->stack->tail_call) {
            // Clear the tail call bit if the call being inlined is not in a
            // tail position in the overall procedure.
            new_instr.flags &= ~SEND_TAIL;
        }
        TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, &result));
    }

    result->message = message;

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
    ResultType result_type)
{
    Blimp *blimp = Optimizer_Blimp(opt);

    *ret = !!(flags & SEND_TAIL);

    SymbolicObject *result;

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
                result_type));

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
                result_type));

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

            if (!blimp->options.inlining ||
                message->value_type == VALUE_UNKNOWN)
            {
                // If inlining is disabled, of course we cannot proceed with the
                // inlining optimization. Also, if the message is not a pure
                // value, we can't substitute the message into the lambda's
                // body, because that might cause the message's side-effects to
                // be evaluated in the wrong order or more than once.
                break;
            }

            // Now that we're going to be processing an inline procedure, we
            // need to create a symbolic stack frame to represent information
            // about this particular send, which will not be on the call stack
            // at runtime since we are eliminating the send.
            SymbolicFrame frame = {
                .flags      = receiver->value.lambda.flags,
                .tail_call  = flags & SEND_TAIL,
                .scope      = NULL,
                .use_result = Optimizer_UseResult(opt, result_type),
                .arg        = message,
                .captures   = receiver->value.lambda.captures,
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
                scope
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
            Optimizer_Delete(opt, message);

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
            *range, flags, ScopedObject_Borrow(call_scope)
        };
        if (!opt->stack->tail_call) {
            // Clear the tail call bit if the call being inlined is not
            // in a tail position in the overall procedure.
            new_instr.flags &= ~SEND_TAIL;
        }
        TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, &result));
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
        TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, &result));
    }

    result->receiver = receiver;
    result->message = message;

    return BLIMP_OK;
}

static Status SymEvalInstruction(
    Optimizer *opt, const Instruction *ip, ScopedObject *scope, bool *ret)
{
    *ret = false;

    SymbolicObject *result;
    switch (ip->type) {
        case INSTR_SYMI: {
            SYMI *instr = (SYMI *)ip;

            // No special optimizations to do for symbol literals; just emit a
            // copy of the instruction.
            TRY(Optimizer_Emit(opt, ip, &result));

            // We do know something about the result though.
            result->value_type = VALUE_SYMBOL;
            result->value.symbol = instr->sym;

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
            TRY(Optimizer_Emit(opt, ip, &result));

            // We know exactly what the result of this instruction will be.
            result->value_type = VALUE_OBJECT;
            result->value.object = instr->object;

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

            if (opt->stack) {
                // If this instruction appears in a procedure that we're
                // inlining, we need to capture the messages corresponding to
                // each frame on the symbolic stack. These correspond to
                // messages which would have been captured by the object's
                // parent, but will not be because after inlining the object's
                // parent is not the scope which immediately contains it
                // (because the scope got inlined) but rather some ancestor
                // thereof.
                //
                // We'll work from in to out, since the symbolic stack is
                // ordered in this way. This means each new capture will be
                // appended to the end of the `captures` list, which is also in
                // in-to-out order.

                // First capture the message being processed by the innermost
                // inline frame (which would have been the new object's
                // immediate parent if not for inlining).
                SymbolicFrame *frame = opt->stack;
                SymbolicObject *capture;
                if (instr->flags & BLOCK_CLOSURE) {
                    TRY(SymEvalObject(opt, frame->arg, RESULT_USE));
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

                // That takes care of the capture which this object would always
                // be responsible for, inlining or no: its parent's message.
                //
                // Now we also have to capture all objects captured by ancestors
                // of the new object which have been inlined. Work our way up
                // the inline stack starting from the innermost frame.
                size_t num_captures = 1;
                    // We need to count the number of captures which we push
                    // onto the result stack. The count starts at 1 because we
                    // already unconditionally pushed one object onto the stack
                    // above.
                while (true) {
                    for (SymbolicObject *capture = frame->captures;
                         capture;
                         capture = capture->next)
                    {
                        TRY(SymEvalObject(opt, capture, RESULT_USE));
                        ++num_captures;

                        SymbolicObject *obj = Optimizer_Pop(opt);
                        assert(obj->next == NULL);
                        assert(captures_end != NULL);
                        captures_end->next = obj;
                        captures_end = obj;
                    }

                    if (frame->scope != NULL || frame->up == NULL) {
                        // If we get to a frame with a scope, we can stop,
                        // because we will use that scope as the new object's
                        // parent, and any messages captured farther up the
                        // stack should already be captured and owned by
                        // `frame->scope`.
                        //
                        // Also, if we've reached the top frame, we must stop,
                        // and in this case the ambient scope which is on the
                        // call stack at runtime will serve as the new object's
                        // parent, fulfilling a similar role as `frame->scope`
                        // in the first case.
                        break;
                    }

                    frame = frame->up;
                }

                // Replace the new block's code with a more optimized version if
                // one is available.
                size_t specialized = instr->specialized;
                Bytecode *code = instr->code;
                ++code->refcount;
                    // We take a new reference to the code because it's going to
                    // be owned by the instruction we emit.
                Optimizer_ReplaceSubroutine(opt, &code, &specialized);

                if (frame->scope == NULL) {
                    // As mentioned above, if we didn't find an inline frame
                    // with an explicit scope, then we have captured all objects
                    // captured by all frames on the inline stack:
                    assert(frame->up == NULL);
                    // Therefore, we can use the scope from the runtime call
                    // stack as the parent of the new object, and we won't miss
                    // any captures. The BLOCKI instruciton implicitly uses the
                    // call stack scope as the parent.
                    BLOCKI new_instr = {
                        {INSTR_BLOCKI, ip->result_type, sizeof(BLOCKI)},
                        instr->msg_name,
                        code,
                        frame->flags | BLOCK_LAMBDA,
                            // Use `frame->flag`, because whether or not we
                            // capture the message being processed by the
                            // current frame on the runtime stack should depend
                            // on the BLOCK_CLOSURE flag from the outermost
                            // inlined scope.
                        specialized,
                        instr->captures + num_captures
                    };
                    TRY(Optimizer_Emit(
                        opt, (Instruction *)&new_instr, &result));
                } else {
                    // If we found a frame with an explicit scope, then we use a
                    // CLOSEI instruction to encode that scope. This instruction
                    // will not require any information from the runtime call
                    // stack.
                    CLOSEI new_instr = {
                        {INSTR_CLOSEI, ip->result_type, sizeof(CLOSEI)},
                        ScopedObject_Borrow(frame->scope),
                        instr->msg_name,
                        code,
                        BLOCK_LAMBDA,
                            // The BLOCK_CLOSURE flag would be ignored for the
                            // CLOSEI instruction. This is alright, because
                            // we've explicitly handled all the captures we need
                            // by
                            //  1. explicitly capturing the argument to the
                            //     innermost inline scope, and
                            //  2. capturing all captures of all inline scopes,
                            //     which include the arguments to the inline
                            //     scope above each scope.
                        specialized,
                        instr->captures + num_captures
                    };
                    TRY(Optimizer_Emit(
                        opt, (Instruction *)&new_instr, &result));
                    result->value.lambda.scope = frame->scope;
                }
            } else {
                // If we're not inlining, we don't have to worry about capturing
                // messages from the inline stack. We will emit a BLOCK
                // instruction which is very nearly a copy of the original
                // instruction:
                BLOCKI new_instr = *instr;

                // Except we replace the bytecode with a more optimized version
                // if one is available.
                ++new_instr.code->refcount;
                Optimizer_ReplaceSubroutine(
                    opt, &new_instr.code, &new_instr.specialized);

                TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, &result));
            }

            // We have some static information about the value of this BLOCKI or
            // CLOSEI instruction (for example, we know it's captures and we
            // know what bytecode procedure it references). This can be useful
            // later on for potentially inlining sends to this object.
            result->captures = captures;
            if (instr->flags & BLOCK_LAMBDA) {
                result->value_type = VALUE_LAMBDA;
                result->value.lambda.msg_name = instr->msg_name;
                result->value.lambda.code = instr->code;
                result->value.lambda.flags = instr->flags;
                result->value.lambda.specialized = instr->specialized;
                result->value.lambda.captures = captures;
            }

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
            Optimizer_ReplaceSubroutine(
                opt, &new_instr.code, &new_instr.specialized);

            // Emit the new instruction.
            ScopedObject_Borrow(instr->scope);
                // We need to borrow the scope object since both the old and new
                // instruction will now reference it.
            TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, &result));

            // Fill in information about the result.
            result->captures = captures;
            if (instr->flags & BLOCK_LAMBDA) {
                result->value_type = VALUE_LAMBDA;
                result->value.lambda.scope = instr->scope;
                result->value.lambda.msg_name = instr->msg_name;
                result->value.lambda.code = instr->code;
                result->value.lambda.flags = instr->flags;
                result->value.lambda.specialized = instr->specialized;
                result->value.lambda.captures = captures;
            }

            return BLIMP_OK;
        }

        case INSTR_MSG: {
            MSG *instr = (MSG *)ip;

            // The index of the message possibly includes messages which are
            // captured on the inline stack. So work our way up the stack from
            // innermost frame to outermost frame counting captures. If we reach
            // `index` captures, then we have our result. Otherwise, we will be
            // retrieving a captured message from the scope on the runtime call
            // stack, with an adjusted index to account for the number of inline
            // captures.
            SymbolicObject *obj = NULL;
            size_t index = instr->index;
            ScopedObject *capturing_scope = NULL;
                // An existing object which captures the desired message, in
                // case we have to emit a MSGOF instruction.
            size_t capturing_scope_index = 0;
                // The offset of indices of messages captured by
                // `capturing_scope` relative to their indices in the innermost
                // scope.
            for (SymbolicFrame *frame = opt->stack; frame; frame = frame->up) {
                // If `index` is 0, the argument is the object we're looking
                // for.
                if (index == 0) {
                    obj = frame->arg;
                    break;
                }

                // Otherwise just move one message farther up the stack.
                assert(capturing_scope == NULL);
                --index;
                ++capturing_scope_index;

                if (capturing_scope == NULL && frame->scope != NULL) {
                    capturing_scope = frame->scope;
                        // If we haven't found the message we're looking for
                        // yet, then it must be captured by this frame or an
                        // ancestor of it. Therefore, if this frame has a scope,
                        // we can potentially use that scope to look up the
                        // message by an adjusted index, if we end up being
                        // unable to find the message on the inline stack.
                }

                // Search each message captured by this frame.
                for (SymbolicObject *capture = frame->captures;
                     capture != NULL;
                     capture = capture->next)
                {
                    if (index == 0) {
                        // If `index` gets to 0, we've found our captured
                        // message.
                        obj = capture;
                        break;
                    }

                    // Otherwise, move one message farther up the stack.
                    --index;
                    if (capturing_scope == NULL) ++capturing_scope_index;
                }
            }

            if (obj == NULL) {
                // If we didn't find the index on the inline stack, get it from
                // the captures of the out-of-line scope.
                obj = Optimizer_GetMessage(opt, index);
            }

            // Try various strategies to evaluate `obj`.
            if (CanSymEvalObject(obj)) {
                // If we know the value of `obj`, just ignore the whole message
                // index business altogether and generate code which produces
                // and equivalent value.
                return SymEvalObject(opt, obj, ip->result_type);
            } else if (capturing_scope != NULL) {
                // If we have a scope which we know captures the message we're
                // looking for, emit a MSGOF instruction to retrieve the message
                // from that scope.
                assert(capturing_scope_index <= instr->index);
                MSGOF new_instr = {
                    {INSTR_MSGOF, ip->result_type, sizeof(MSGOF)},
                    ScopedObject_Borrow(capturing_scope),
                    instr->index - capturing_scope_index
                        // The index is offset by the difference between indices
                        // in the current scope and indices in the capturing
                        // scope.
                };
                TRY(Optimizer_Emit(
                    opt, (Instruction *)&new_instr, &result));
            } else if (opt->stack == NULL || index != 0) {
                // If we're not inlining, or if `index` refers to a message
                // which is not on the inline stack, then get it from the scope
                // on the runtime call stack.
                MSG new_instr = {
                    {INSTR_MSG, ip->result_type, sizeof(MSG)}, index
                };
                TRY(Optimizer_Emit(opt, (Instruction *)&new_instr, &result));
            } else {
                // Finally, if the message we want is on the inline stack, we
                // can't retrieve it using a MSG or MSGOF instruction. Since we
                // also couldn't evaluate the message to produce an equivalent
                // object, we are stuck. We cannot generate code which is
                // equivalent to this MSG instruction.
                //
                // However, we can try emitting a ghost object. Since we likely
                // know something about the value of this object, downstream
                // optimizations may be able to optimize it away entirely, and
                // then the fact that we can't evaluate this MSG instruction
                // inline won't matter.
                //
                // If we fail to optimize away this ghost object later, the
                // worst that will happen is we will abandon the idea of
                // inlining this procedure and generate code for an out-of-line
                // send instead.
                TRY(Optimizer_EmitGhost(opt, ip->result_type, &result));
            }

            SymbolicObject_CopyValue(obj, result);
                // Whatever method we used to evaluate the object, the new
                // object has the same value as the old object.
            return BLIMP_OK;
        }

        case INSTR_MSGOF: {
            MSGOF *instr = (MSGOF *)ip;

            ScopedObject_Borrow(instr->scope);
            TRY(Optimizer_Emit(opt, ip, &result));

            return BLIMP_OK;
        }

        case INSTR_SEND: {
            SEND *instr = (SEND *)ip;

            SymbolicObject *message = Optimizer_Pop(opt);
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
                ip->result_type);
        }

        case INSTR_CALL: {
            CALL *instr = (CALL *)ip;

            SymbolicObject *message = Optimizer_Pop(opt);
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
                ip->result_type);
        }

        case INSTR_SENDTO: {
            SENDTO *instr = (SENDTO *)ip;

            return SymEvalSendTo(
                opt,
                false,
                instr->receiver,
                Optimizer_Pop(opt),
                &instr->range,
                instr->flags,
                scope,
                ret,
                ip->result_type);
        }

        case INSTR_CALLTO: {
            CALLTO *instr = (CALLTO *)ip;

            return SymEvalSendTo(
                opt,
                true,
                instr->receiver,
                Optimizer_Pop(opt),
                &instr->range,
                instr->flags,
                instr->scope,
                ret,
                ip->result_type);
        }

        case INSTR_RET: {
            *ret = true;

            if (opt->stack && !opt->stack->tail_call) {
                // If we encounter a RET in a procedure being inlined, don't
                // actually emit a RET, since that would return from the parent
                // procedure that we're inlined into.
                return BLIMP_OK;
            }

            return Optimizer_Emit(opt, ip, &result);
        }

        case INSTR_NOP: {
            return Optimizer_Emit(opt, ip, &result);
        }
    }

    assert(false);
    return Error(Optimizer_Blimp(opt), BLIMP_ERROR);
}

static Status SymEvalProcedure(
    Optimizer *opt, Bytecode *code, ScopedObject *scope)
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
        if (SymEvalInstruction(opt, ip, scope, &ret) != BLIMP_OK) {
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
    Status status = SymEvalProcedure(opt, code, scope);
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
    Status status = SymEvalProcedure(opt, code, scope);
    Optimizer_Pop(opt);
        // Clean up the final result from the result stack.
    Optimizer_End(opt, optimized);

    return status;
}
