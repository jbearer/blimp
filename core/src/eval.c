#include "internal/blimp.h"
#include "internal/eval.h"

// Execute a SEND. If the SEND results in a jump to another procedure (for
// example, a send to a block object causes execution to jump to that object's
// message handler) then the runtime is prepared to execute that procedure, and
// `*jump_to` is set to the address of the instruction to execute next, so that
// the send can be completed by calling ExecuteFrom(blimp, *jump_to, &result).
//
// Otherwise, if the result of the SEND can be determined immediately (for
// example, the result of sending to a reference is always the symbol associated
// with the reference) then it is pushed on top of the result stack if
// `use_result`, and *jump_to is set to NULL.
//
// This function consumes references to `receiver` and `message`: the caller
// must own references to both of these objects, and the moment ExecuteSend is
// called, ownership is taken away from the caller. It does not consume the
// reference to `scope`: the caller is still responsible for cleaning up that
// reference.
static Status ExecuteSend(
    Blimp *blimp,
    ScopedObject *scope,
    Object *receiver,
    Object *message,
    bool use_result,
    const SourceRange *range,
    const Instruction *return_address,
    const Instruction **jump_to)
{
    // A send to a symbol always results in a send to another object (either to
    // the value of the symbol, if it is initialized, or to the initializer sent
    // to the symbol, if the symbol is just now to be initialized). Therefore,
    // we can handle the case where `receiver` is a symbol by dereferencing the
    // symbol and updating `receiver` with the new object which will receive a
    // message, iterating until `receiver` is no longer a symbol.
    while (Object_Type(receiver) == OBJ_SYMBOL) {
        const Symbol *sym = (const Symbol *)receiver;

        Object *value;
        if (ScopedObject_Get(scope, sym, &value) == BLIMP_OK) {
            receiver = BlimpObject_Borrow(value);
                // If `sym` has a value in this scope, then we simply forward
                // the same message to that value.
        } else {
            // If `sym` is not in scope, we create a reference to `sym` which
            // can be used to initialize it...
            ReferenceObject *ref;
            if (ReferenceObject_New(blimp, scope, sym, &ref) != BLIMP_OK) {
                BlimpObject_Release(message);
                return Reraise(blimp);
            }

            // ...and we pass that reference to the message which was passed to
            // `sym`.
            receiver = message;
            message = (Object *)ref;
        }
    }

    Status status = BLIMP_OK;
    *jump_to = NULL;

    // Now we know that the receiver is not a symbol. We have three cases: the
    // receiver is one of OBJ_REFERENCE, OBJ_BLOCK, or OBJ_EXTENSION. Each of
    // these object types reacts in a different way when it receives a message.
    switch (Object_Type(receiver)) {
        case OBJ_REFERENCE: {
            ReferenceObject *ref = (ReferenceObject *)receiver;

            // When a reference object receives a message, it sets
            // the value of the associated symbol to the message.
            if ((status = ReferenceObject_Store(ref, message)) != BLIMP_OK) {
                break;
            }

            // The result is the symbol associated with the reference.
            if (use_result) {
                if ((status = ObjectStack_Push(
                        blimp,
                        &blimp->result_stack,
                        (Object *)ref->symbol)) != BLIMP_OK)
                {
                    break;
                }
            }

            break;
        }

        case OBJ_BLOCK: {
            BlockObject *block = (BlockObject *)receiver;

            // When a block object receives a message, we transfer control to
            // the block's message handler procedure. Push a new stack frame to
            // hold the local context of the block's procedure.
            ++block->code->refcount;
            StackFrame frame = {
                .return_address = return_address,
                .scope          = (ScopedObject *)block,
                .message        = message,
                .has_range      = range != NULL,
                .use_result     = use_result,
                .executing      = block->code,
            };
            if (range != NULL) {
                frame.range = *range;
            }
            if ((status = Stack_Push(blimp, &blimp->stack, &frame, 0))
                            // We request 0 additional bytes from the C call
                            // stack, because adding a frame to the bl:mp call
                            // stack does not correspond to descending an
                            // additional level in the C stack. In fact, we are
                            // instead going to return up a frame from this
                            // function, and then ExecuteFrom will continue
                            // executing the new procedure call in an iterative
                            // fashion, using only a single frame on the C call
                            // stack.
                    != BLIMP_OK)
            {
                break;
            }

            // Jump to the first instruction in the message handler.
            *jump_to = Bytecode_Begin(block->code);
            return BLIMP_OK;
                // Don't break; we don't want to release the references to
                // message and receiver, since they are now owned by the stack
                // frame we just pushed.
        }

        case OBJ_EXTENSION: {
            ExtensionObject *ext = (ExtensionObject *)receiver;

            // When an extension object receives a message, we call its method
            // to compute the result of the send.
            Object *obj;
            if ((status = ext->method(
                        blimp,
                        (Object *)scope,
                        receiver,
                        message,
                        &obj))
                    != BLIMP_OK)
            {
                break;
            }

            if (use_result) {
                if ((status = ObjectStack_Push(
                        blimp, &blimp->result_stack, obj)) != BLIMP_OK)
                {
                    BlimpObject_Release(obj);
                    break;
                }
            } else {
                // Extension methods always return a new reference to an object.
                // If we are going to ignore the result, then we need to release
                // the reference that we now own.
                BlimpObject_Release(obj);
            }

            break;
        }

        default:
            status = Error(blimp, BLIMP_INVALID_OBJECT_TYPE);
            break;
    }

    if (status != NULL && !status->has_range && range != NULL) {
        // A lot of extension methods do not assign source locations to their
        // errors. If this was one of those methods, or if for some other reason
        // we got an error with no location, set the location to the location of
        // the overall send expression.
        status->range = *range;
        status->has_range = true;
    }

    BlimpObject_Release(receiver);
    BlimpObject_Release(message);
    return status;
}

static Status EvalSend(
    Blimp *blimp,
    Object *receiver,
    Object *message,
    const SourceRange *range,
    SendFlags flags,
    ScopedObject *scope,
    bool use_result,
    const StackFrame **sp,
    const Instruction **ip,
    bool *ret,
    Object **result)
{
    *ret = false;

    bool tail_call = !!(flags & SEND_TAIL);

    // Save what we need from the stack frame in case this is a tail call and we
    // are about to pop the stack frame.
    const Instruction *return_address =
        tail_call ? (*sp)->return_address
                  : Instruction_Next(*ip);
    SourceRange range_copy = *range;

    if (tail_call) {
        if ((*sp)->message) BlimpObject_Release((*sp)->message);
        if ((*sp)->scope) BlimpObject_Release((Object*)(*sp)->scope);
        if ((*sp)->executing) BlimpBytecode_Free((*sp)->executing);
        Stack_Pop(blimp, &blimp->stack);
        *sp = Stack_CurrentFrame(&blimp->stack);
    }

    if (ExecuteSend(
            blimp,
            scope,
            receiver,
            message,
            use_result,
            &range_copy,
            return_address,
            ip)
        != BLIMP_OK)
    {
        *ret = return_address == NULL;
            // If we would have returned had this call been succesful, then we
            // don't want the caller to unwind the stack when we raise an error.
            // If they were to do that, they might miss the frame that causes a
            // return to user code (if we already popped that frame as part of a
            // tail call) and unwind too far.

        BlimpObject_Release((Object *)scope);
        return Reraise(blimp);
    }
    BlimpObject_Release((Object *)scope);

    // Update the instruction pointer and stack pointer.
    *sp = Stack_CurrentFrame(&blimp->stack);
    if (*ip == NULL) {
        *ip = return_address;
        if (*ip == NULL) {
            *ret = true;
            if (result != NULL) {
                *result = ObjectStack_Pop(blimp, &blimp->result_stack);
            }

            return BLIMP_OK;
        }
    }

    return BLIMP_OK;
}

static Status TryConstantElision(
    Blimp *blimp,
    Object **receiver_ptr,
    ScopedObject *scope,
    Object **tmp_receiver)
{
    Object *receiver = BlimpObject_Borrow(*receiver_ptr);

    // Dereference symbols. As long as we keep dereferencing constant symbols,
    // we can perform constant elision (if it is enabled) by replacing the
    // receiver in the instruction with the object referred to by that symbol,
    // to save a hash lookup next time this instruction is executed.
    //
    // Even if we dereference a non-constant symbol, or if constant elision is
    // not enabled, this is still useful work. We can return the ultimate value
    // of the symbol (const or otherwise) in `tmp_receiver`, which will not
    // update the instruction in the procedure, but will use the fully
    // dereferenced receiver for the execution of this send.
    ScopedObject *owner;
        // Scope owning the receiver symbol.
    bool is_const = true;
        // Whether all the symbols we have dereferenced so far are
        // constant.
    bool receiver_is_const;
        // Whether the most recently dereferenced symbol is
        // constant.
    while (
        Object_Type(receiver) == OBJ_SYMBOL &&

        // Check if the symbol is in scope, and, if so, whether it
        // is constant.
        ScopedObject_Lookup(
            scope,
            (const Symbol *)receiver,
            &receiver,
            &owner,
            &receiver_is_const)
    ) {
        is_const &= receiver_is_const;

        BlimpObject_Borrow(receiver);
        if (blimp->options.constant_elision &&
            Object_Type((Object *)scope) == OBJ_BLOCK && is_const)
        {
            if (BlockObject_IsSpecialized((BlockObject *)scope, owner))
            {
                // If the code we're executing is already specialized in the
                // scope where the symbol is defined, then we can simply update
                // the instruction in place.
                BlimpObject_Borrow(receiver);
                BlimpObject_Release(*receiver_ptr);
                *receiver_ptr = receiver;
            } else {
                // Otherwise, we must first specialize the code so that the
                // constant-elided code only runs in the scope containing the
                // constant value (or in a descendant of that scope).
                //
                // The specialization process automatically performs scope-aware
                // optimizations including constant elision, so all we have to
                // do is request for the object to be specialized.
                if (BlockObject_Specialize((BlockObject *)scope, owner)
                        != BLIMP_OK)
                {
                    BlimpObject_Release(receiver);
                    return Reraise(blimp);
                }
            }
        }
    }

    *tmp_receiver = receiver;
    return BLIMP_OK;
}

// Start executing bytecode at the given instruction. Execution will continue
// until a RET instruction executes in a stack frame with a NULL return address.
// At that point, there must be at least one object on the top of the result
// stack. That object will be removed from the stack and stored in `result`, and
// ExecuteFrom will return.
//
// It is the caller's responsibility to ensure that there is a frame on the call
// stack with a NULL return address when this function is called, and to ensure
// that the bytecode being executed is valid and that the result stack contains
// any necessary inputs that the bytecode procedure expects.
//
// Note that this function will handle freeing all of the existing stack frames
// up to and including the one with the NULL return address. Therefore, if the
// caller is creating the NULL return stack frame themselves, they must allow
// for ExecuteFrom to release one reference to `scope` and one reference to
// `caller` when that frame is popped from the stack.
static Status ExecuteFrom(Blimp *blimp, const Instruction *ip, Object **result)
{
    const StackFrame *sp = Stack_CurrentFrame(&blimp->stack);
    size_t initial_result_stack_size = ObjectStack_Size(&blimp->result_stack);
        // Save the number of objects on the result stack when we start, so that
        // if we encounter an error, we can remove any objects from the result
        // stack which were added during this function call.

    assert(sp->return_address == NULL);
    assert(sp->use_result == (result != NULL));

    bool ret = false;
    while (true) {
        if (HandleSignals(&blimp->signals) != BLIMP_OK) {
            return Reraise(blimp);
        }

        bool use_result =
            ip->result_type == RESULT_USE ||
            (ip->result_type == RESULT_INHERIT && sp->use_result);

        switch (ip->type) {
            case INSTR_SYMI: {
                if (use_result) {
                    SYMI *instr = (SYMI *)ip;
                    if (ObjectStack_Push(
                            blimp, &blimp->result_stack, (Object *)instr->sym)
                                // Technically we should borrow `sym` before
                                // pushing it onto the stack, but symbols are
                                // not garbage collected so borrowing does
                                // nothing anyways.
                        != BLIMP_OK)
                    {
                        goto error;
                    }
                }

                break;
            }

            case INSTR_BLOCKI: {
                if (use_result) {
                    BLOCKI *instr = (BLOCKI *)ip;

                    // Create a new block object.
                    BlockObject *obj;
                    if (BlockObject_New(
                            blimp,
                            sp->scope,
                            instr->msg_name,
                            instr->code,
                            instr->specialized,
                            &obj) != BLIMP_OK)
                    {
                        goto error;
                    }

                    // Pop captured messages from the result stack.
                    for (size_t i = 0; i < instr->captures; ++i) {
                        Object *capture = ObjectStack_Pop(
                            blimp, &blimp->result_stack);

                        if (ScopedObject_CaptureMessage(
                                (ScopedObject *)obj, capture) != BLIMP_OK)
                        {
                            if (capture) BlimpObject_Release(capture);
                            BlimpObject_Release((Object *)obj);
                            goto error;
                        }

                        if (capture) BlimpObject_Release(capture);
                    }

                    if (sp->message != NULL) {
                        // Capture the parent's message.
                        if (ScopedObject_CaptureMessage(
                                (ScopedObject *)obj,
                                (instr->flags & BLOCK_CLOSURE)
                                    ? sp->message
                                    : NULL)
                            != BLIMP_OK)
                        {
                            BlimpObject_Release((Object *)obj);
                            goto error;
                        }
                    }

                    // Push the object onto the stack. Note that we do not
                    // borrow it first: we own a reference to `obj` after
                    // BlockObject_New, and we are giving this reference away to
                    // the stack.
                    if (ObjectStack_Push(
                            blimp, &blimp->result_stack, (Object *)obj)
                        != BLIMP_OK)
                    {
                        BlimpObject_Release((Object *)obj);
                        goto error;
                    }
                }

                break;
            }

            case INSTR_CLOSEI: {
                if (use_result) {
                    CLOSEI *instr = (CLOSEI *)ip;

                    // Create a new block object.
                    BlockObject *obj;
                    if (BlockObject_New(
                            blimp,
                            instr->scope,
                            instr->msg_name,
                            instr->code,
                            instr->specialized,
                            &obj) != BLIMP_OK)
                    {
                        goto error;
                    }

                    // Pop captured messages from the result stack.
                    for (size_t i = 0; i < instr->captures; ++i) {
                        Object *capture = ObjectStack_Pop(
                            blimp, &blimp->result_stack);

                        if (ScopedObject_CaptureMessage(
                                (ScopedObject *)obj, capture) != BLIMP_OK)
                        {
                            if (capture) BlimpObject_Release(capture);
                            BlimpObject_Release((Object *)obj);
                            goto error;
                        }

                        if (capture) BlimpObject_Release(capture);
                    }

                    // Push the object onto the stack. Note that we do not
                    // borrow it first: we own a reference to `obj` after
                    // BlockObject_New, and we are giving this reference away to
                    // the stack.
                    if (ObjectStack_Push(
                            blimp, &blimp->result_stack, (Object *)obj)
                        != BLIMP_OK)
                    {
                        BlimpObject_Release((Object *)obj);
                        goto error;
                    }
                }

                break;
            }

            case INSTR_OBJI: {
                OBJI *instr = (OBJI *)ip;

                if (instr->object != NULL) {
                    BlimpObject_Borrow(instr->object);
                }

                if (ObjectStack_Push(
                        blimp, &blimp->result_stack, instr->object) != BLIMP_OK)
                {
                    if (instr->object != NULL) {
                        BlimpObject_Release(instr->object);
                    }
                    goto error;
                }

                break;
            }

            case INSTR_MSG: {
                if (use_result) {
                    MSG *instr = (MSG *)ip;
                    Object *obj;
                    if (instr->index == 0) {
                        // If the message referred to by the index is the
                        // message we are processing right now, then it is not
                        // captured by the `scope` closure, since it is being
                        // passed as an argument to `scope`. Therefore, we have
                        // to get it from the call stack.
                        obj = sp->message;
                    } else {
                        // Otherwise, the message is captured by `scope`. We
                        // have to offset the index by 1, because the index
                        // encoded in the instruction counts the message
                        // currently being processed, but `scope`s list of
                        // captures does not.
                        if (ScopedObject_GetCapturedMessage(
                                sp->scope, instr->index - 1, &obj) != BLIMP_OK)
                        {
                            goto error;
                        }
                    }

                    // Push the message onto the result stack. We need to borrow
                    // it, because
                    //  * If we got it from the stack frame, then the stack
                    //    frame still expects to own a reference to it, so we
                    //    need to borrow a new reference to give to the result
                    //    stack.
                    //  * Otherwise, ScopedObject_GetCapturedMessage() returns a
                    //    transient reference. Since the object needs to persist
                    //    for an arbitrarily long time while it is on the result
                    //    stack, we need to upgrade this to a new reference.
                    if (ObjectStack_Push(
                            blimp,
                            &blimp->result_stack,
                            BlimpObject_Borrow(obj))
                        != BLIMP_OK)
                    {
                        BlimpObject_Release(obj);
                        goto error;
                    }
                }

                break;
            }

            case INSTR_MSGOF: {
                if (use_result) {
                    MSGOF *instr = (MSGOF *)ip;

                    // Get the captured message from the object indicated in the
                    // instruction.
                    Object *obj;
                    if (ScopedObject_GetCapturedMessage(
                            instr->scope, instr->index, &obj) != BLIMP_OK)
                    {
                        goto error;
                    }

                    // Push it onto the stack. We need to borrow it because
                    // ScopedObject_GetCapturedMessage() returns a transient
                    // reference.
                    if (ObjectStack_Push(
                            blimp,
                            &blimp->result_stack,
                            BlimpObject_Borrow(obj))
                        != BLIMP_OK)
                    {
                        BlimpObject_Release(obj);
                        goto error;
                    }
                }

                break;
            }

            case INSTR_SEND: {
                SEND *instr = (SEND *)ip;

                // Get the receiver and message from the result stack. The
                // message is computed and pushed onto the stack after the
                // receiver, which means it is the first object to pop off the
                // stack.
                Object *message  = ObjectStack_Pop(blimp, &blimp->result_stack);
                Object *receiver = ObjectStack_Pop(blimp, &blimp->result_stack);

                if (EvalSend(
                        blimp,
                        receiver,
                        message,
                        &instr->range,
                        instr->flags,
                        (ScopedObject *)BlimpObject_Borrow((Object *)sp->scope),
                        use_result,
                        &sp,
                        &ip,
                        &ret,
                        result)
                    != BLIMP_OK)
                {
                    goto error;
                }

                if (ret) {
                    return BLIMP_OK;
                } else {
                    continue;
                }
            }

            case INSTR_CALL: {
                CALL *instr = (CALL *)ip;

                // Get the receiver and message from the result stack. The
                // message is computed and pushed onto the stack after the
                // receiver, which means it is the first object to pop off the
                // stack.
                Object *message  = ObjectStack_Pop(blimp, &blimp->result_stack);
                Object *receiver = ObjectStack_Pop(blimp, &blimp->result_stack);

                if (EvalSend(
                        blimp,
                        receiver,
                        message,
                        &instr->range,
                        instr->flags,
                        (ScopedObject *)BlimpObject_Borrow(
                            (Object *)instr->scope),
                        use_result,
                        &sp,
                        &ip,
                        &ret,
                        result)
                    != BLIMP_OK)
                {
                    goto error;
                }

                if (ret) {
                    return BLIMP_OK;
                } else {
                    continue;
                }
            }

            case INSTR_SENDTO: {
                SENDTO *instr = (SENDTO *)ip;

                Object *receiver;
                if (TryConstantElision(
                        blimp, &instr->receiver, sp->scope, &receiver)
                    != BLIMP_OK)
                {
                    goto error;
                }

                // Get the message from the result stack.
                Object *message = ObjectStack_Pop(blimp, &blimp->result_stack);

                if (EvalSend(
                        blimp,
                        receiver,
                        message,
                        &instr->range,
                        instr->flags,
                        (ScopedObject *)BlimpObject_Borrow((Object *)sp->scope),
                        use_result,
                        &sp,
                        &ip,
                        &ret,
                        result)
                    != BLIMP_OK)
                {
                    goto error;
                }

                if (ret) {
                    return BLIMP_OK;
                } else {
                    continue;
                }
            }

            case INSTR_CALLTO: {
                CALLTO *instr = (CALLTO *)ip;

                Object *receiver;
                if (TryConstantElision(
                        blimp, &instr->receiver, instr->scope, &receiver)
                    != BLIMP_OK)
                {
                    goto error;
                }

                // Get the message from the result stack.
                Object *message = ObjectStack_Pop(blimp, &blimp->result_stack);

                if (EvalSend(
                        blimp,
                        receiver,
                        message,
                        &instr->range,
                        instr->flags,
                        (ScopedObject *)BlimpObject_Borrow(
                            (Object *)instr->scope),
                        use_result,
                        &sp,
                        &ip,
                        &ret,
                        result)
                    != BLIMP_OK)
                {
                    goto error;
                }

                if (ret) {
                    return BLIMP_OK;
                } else {
                    continue;
                }
            }

            case INSTR_RET: {
                // We're about to pop this stack frame, so release the
                // references that it owns.
                if (sp->scope)     BlimpObject_Release((Object *)sp->scope);
                if (sp->message)   BlimpObject_Release(sp->message);
                if (sp->executing) BlimpBytecode_Free(sp->executing);

                ip = sp->return_address;
                    // Continue executing from the return address stored in the
                    // stack frame.

                // Update the stack.
                Stack_Pop(blimp, &blimp->stack);
                sp = Stack_CurrentFrame(&blimp->stack);

                if (ip == NULL) {
                    // A NULL return address is a signal that we should return a
                    // result to our caller.
                    if (result != NULL) {
                        *result = ObjectStack_Pop(blimp, &blimp->result_stack);
                    }

                    return BLIMP_OK;
                } else {
                    continue;
                }
            }

            case INSTR_NOP: {
                break;
            }
        }

        ip = Instruction_Next(ip);
    }

error:
    // Release objects that we added to the result stack.
    while (ObjectStack_Size(&blimp->result_stack) > initial_result_stack_size) {
        BlimpObject_Release(ObjectStack_Pop(blimp, &blimp->result_stack));
    }

    // Unwind the stack until we get to the frame which would have caused this
    // function to return. `ret` is initially set only if we already popped such
    // a stack frame as part of a tail call which later failed.
    while (!ret) {
        if (sp->scope)     BlimpObject_Release((Object *)sp->scope);
        if (sp->message)   BlimpObject_Release(sp->message);
        if (sp->executing) BlimpBytecode_Free(sp->executing);

        ret = sp->return_address == NULL;
        Stack_Pop(blimp, &blimp->stack);
        sp = Stack_CurrentFrame(&blimp->stack);
    }

    return Reraise(blimp);
}

Status EvalBytecode(
    Blimp *blimp, ScopedObject *scope, Bytecode *code, Object **result)
{
    BlimpObject_Borrow((Object *)scope);
        // We have to borrow `scope`, because the stack frame that we're about
        // to create will own a reference to it, but the reference that we
        // currently have belongs to our caller and must remain valid even after
        // the reference owned by the stack frame is released.
    ++code->refcount;
        // We also have to borrow the code we're going to execute, as the stack
        // frame owns a reference to that as well.

    StackFrame frame = {
         .return_address = NULL,
            // A NULL return address indicates that RET instructions should
            // cause us to return from this function back into user code, rather
            // than returning to the address stored in the stack frame.
        .scope          = scope,
        .message        = NULL,
        .has_range      = false,
        .use_result     = result != NULL,
        .executing      = code,
    };
    if (Stack_Push(blimp, &blimp->stack, &frame, 128) != BLIMP_OK) {
            // Unlike in ExecuteSend, where we requested 0 additional bytes on
            // the C call stack when pushing a frame to the bl:mp call stack,
            // here we request someting nonzero (128 bytes is fairly arbitrary)
            // to account for the fact that we are using one C stack frame here
            // and one for ExecuteFrom().
        BlimpObject_Release((Object *)scope);
        BlimpBytecode_Free(code);
        return Reraise(blimp);
    }

    EnableSignals(&blimp->signals);
        // We're about to start executing, which means we need to prepare to get
        // interrupted if the user calls Blimp_RaiseSignal().

    if (ExecuteFrom(blimp, Bytecode_Begin(code), result) != BLIMP_OK) {
        if (Stack_Empty(&blimp->stack)) {
            // If we're returning from the top stack frame, execution is
            // finished, so we don't need to handle interruptions any more.
            //
            // Since we're returning with an error, we won't pause to execute
            // pending signal handlers; thence ClearAndDisableSignals() rather
            // than DisableSignals().
            ClearAndDisableSignals(&blimp->signals);
        }
        return Reraise(blimp);
    } else {
        if (Stack_Empty(&blimp->stack)) {
            assert(ObjectStack_Empty(&blimp->result_stack));

            // If we're returning from the top stack frame, execution is
            // finished, so we don't need to handle interruptions any more.
            return DisableSignals(&blimp->signals);
        } else {
            return BLIMP_OK;
        }
    }
}

PRIVATE Status Send(
    Blimp *blimp,
    ScopedObject *scope,
    Object *receiver,
    Object *message,
    Object **result)
{
    // We need to borrow `receiver` and `message`, because ExecuteSend consumes
    // them, but the references we currently have are owned by our caller and
    // must remain valid even after ExecuteSend.
    BlimpObject_Borrow(receiver);
    BlimpObject_Borrow(message);

    const Instruction *jump_to;
    if (ExecuteSend(
            blimp,
            scope,
            receiver,
            message,
            result != NULL,
                // Only compute a result if we have somewhere to put it.
            NULL,
                // No source range.
            NULL,
                // By passing a NULL return address, we ensure that if
                // ExecuteSend causes a procedure call, then the top frame on
                // the call stack will have a NULL return address, so then
                // ExecuteFrom will return once it has processed that procedure
                // call.
            &jump_to)
        != BLIMP_OK)
    {
        return Reraise(blimp);
    }
    if (jump_to) {
        // If the send resulted in a procedure call, execute the procedure.
        if (ExecuteFrom(blimp, jump_to, result) != BLIMP_OK) {
            return Reraise(blimp);
        }
    } else {
        // Otherwise, the result should be stored on top of the result stack.
        if (result != NULL) {
            *result = ObjectStack_Pop(blimp, &blimp->result_stack);
        }
    }

    return BLIMP_OK;
}
