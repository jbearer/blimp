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
            StackFrame frame = {
                .return_address = return_address,
                .scope          = (ScopedObject *)block,
                .message        = message,
                .has_range      = range != NULL,
                .use_result     = use_result
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

    if (status && !status->has_range) {
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

    while (true) {
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
                            instr->capture_parents_message,
                            &obj) != BLIMP_OK)
                    {
                        goto error;
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

            case INSTR_SEND:
            case INSTR_RSEND: {
                SEND *instr = (SEND *)ip;
                bool tail_call = ip->type == INSTR_RSEND;

                // Get the receiver and message from the result stack. The
                // message is computed and pushed onto the stack after the
                // receiver, which means it is the first object to pop off the
                // stack.
                Object *message  = ObjectStack_Pop(blimp, &blimp->result_stack);
                Object *receiver = ObjectStack_Pop(blimp, &blimp->result_stack);

                // Save what we need from the stack frame in case this is a tail
                // call and we are about to pop the stack frame.
                ScopedObject *scope = sp->scope;
                const Instruction *return_address =
                    tail_call ? sp->return_address
                              : Instruction_Next(ip);

                if (tail_call) {
                    if (sp->message) BlimpObject_Release(sp->message);
                        // We don't need the message from the stack frame
                        // anymore. We cannot yet release `sp->scope`, though,
                        // because we're still using it as the scope argument to
                        // ExecuteSend. We will release it once that function
                        // finishes.
                    Stack_Pop(blimp, &blimp->stack);
                }

                Status status = ExecuteSend(
                    blimp,
                    sp->scope,
                    receiver,
                    message,
                    use_result,
                    &instr->range,
                    return_address,
                    &ip);
                // Clean up scope if we're returning.
                if (tail_call) {
                    BlimpObject_Release((Object *)scope);
                }
                if (status != BLIMP_OK) {
                    goto error;
                }

                // Update the instruction pointer and stack pointer.
                if (ip == NULL) {
                    ip = return_address;
                    if (ip == NULL) {
                        if (result != NULL) {
                            *result = ObjectStack_Pop(
                                blimp, &blimp->result_stack);
                        }

                        return BLIMP_OK;
                    }
                }
                sp = Stack_CurrentFrame(&blimp->stack);

                // Start executing from the new IP.
                continue;
            }

            case INSTR_RET: {
                // We're about to pop this stack frame, so release the
                // references that it owns.
                if (sp->scope)   BlimpObject_Release((Object *)sp->scope);
                if (sp->message) BlimpObject_Release(sp->message);

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
        }

        ip = Instruction_Next(ip);
    }

error:
    // Release objects that we added to the result stack.
    while (ObjectStack_Size(&blimp->result_stack) > initial_result_stack_size) {
        BlimpObject_Release(ObjectStack_Pop(blimp, &blimp->result_stack));
    }

    // Unwind the stack until we get to the frame which would have caused this
    // function to return.
    while (true) {
        if (sp->scope)   BlimpObject_Release((Object *)sp->scope);
        if (sp->message) BlimpObject_Release(sp->message);
        bool last_frame = sp->return_address == NULL;
        Stack_Pop(blimp, &blimp->stack);

        if (last_frame) {
            break;
        } else {
            sp = Stack_CurrentFrame(&blimp->stack);
        }
    }

    return Reraise(blimp);
}

Status EvalBytecode(
    Blimp *blimp, ScopedObject *scope, const Bytecode *code, Object **result)
{
    BlimpObject_Borrow((Object *)scope);
        // We have to borrow `scope`, because the stack frame that we're about
        // to create will own a reference to it, but the reference that we
        // currently have belongs to our caller and must remain valid even after
        // the reference owned by the stack frame is released.

    StackFrame frame = {
         .return_address = NULL,
            // A NULL return address indicates that RET instructions should
            // cause us to return from this function back into user code, rather
            // than returning to the address stored in the stack frame.
        .scope          = scope,
        .message        = NULL,
        .has_range      = false,
        .use_result     = result != NULL
    };
    if (Stack_Push(blimp, &blimp->stack, &frame, 128) != BLIMP_OK) {
            // Unlike in ExecuteSend, where we requested 0 additional bytes on
            // the C call stack when pushing a frame to the bl:mp call stack,
            // here we request someting nonzero (128 bytes is fairly arbitrary)
            // to account for the fact that we are using one C stack frame here
            // and one for ExecuteFrom().
        BlimpObject_Release((Object *)scope);
        return Reraise(blimp);
    }

    return ExecuteFrom(blimp, Bytecode_Begin(code), result);
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
        Object *obj = ObjectStack_Pop(blimp, &blimp->result_stack);
        if (result != NULL) {
            *result = obj;
        } else {
            BlimpObject_Release(obj);
        }
    }

    return BLIMP_OK;
}