////////////////////////////////////////////////////////////////////////////////
// The bl:mp Optimizer
//
// The optimizer takes as input a sequence of bytecode instructions and produces
// a new sequence of instructions which is guaranteed to have the same
// observable behavior as the input, but which is possibly more performant.
//
// The optimizer is essentially a symbolic executer of bl:mp programs. It runs
// through a sequence of unoptimized instructions, maintaining analogs of the
// state maintained by the runtime interpreter which symbolically represent the
// information which is known about that state at optimization time. This state
// includes the result stack and the call stack, the two key pieces of state
// maintained by the runtime interpreter.
//
// This header file defines data structures which symbolically represent the
// result stack and call stack. It also defines the global Optimizer data
// structure and a high-level interface for manipulating symbolic state. An
// implementation of the high-level interface lives in optimizer.c. The symbolic
// interpreter itself lives in sym_eval.c.

#ifndef BLIMP_OPTIMIZER_H
#define BLIMP_OPTIMIZER_H

#include "internal/checkpoint_alloc.h"
#include "internal/common.h"
#include "internal/instruction.h"
#include "internal/result_stack.h"

////////////////////////////////////////////////////////////////////////////////
// Symbolically evaluated objects
//
// Just as the runtime interpreter has a stack of Objects containing the results
// of previous computations, the symbolic interpreter has a stack of
// SymbolicObjects, each of which represents all the information that is known
// statically about the Object which would be in the corresponding position on
// the result stack at runtime.
//
// Notes on ownership and lifetime:
//  * SymbolicObjects may contain references to other data structures which are
//    normal managed or references counted (for examples, references to Objects
//    and Bytecode procedures). These references from SymbolicObjects are not
//    managed or reference counted, because a SymbolicObject only lives for the
//    duration of one pass of the optimizer. The interpreter is effectively
//    paused while the optimizer is running, and the optimizer itself does not
//    destroy any existing references, so there is no chance that these data
//    structures will disappear while the SymbolicObject exists. The references
//    from the SymbolicObject can therefore be thought of as non-owning, weak
//    references which are guaranteed by the design of the interpreter to always
//    remain valid.
//  * Even though there will come a time during an optimization pass when a
//    SymbolicObject allocated during that pass is no longer needed, we make no
//    attempt to deallocate it until the entire pass is finished, when we
//    unconditionally deallocate all objects allocated during that pass. This
//    means we can share SymbolicObjects in complicated ways without having to
//    keep track of all the live references to them. Since any individual
//    optimization pass should be relatively short, this shouldn't put an undue
//    memory burden on the system.
// These property greatly simplify the management of SymbolicObjects: since
// SymbolicObjects do not own references to other data structures, we don't have
// to do any work to release references when a SymbolicObject is destroyed. And
// since, in addition, we don't ever worry about freeing the memory allocated
// for individual SymbolicObjects, we don't have to track at all when
// SymbolicObjects are dead; we just wait until the optimization pass finishes
// and then deallocate the whole batch of objects at once.

#define UNKNOWN_INSTR_OFFSET ((size_t)-1)
#define GHOST_INSTR_OFFSET ((size_t)-2)

typedef struct SymbolicObject {
    size_t instr_offset;
        // The offset in the current procedure of the instruction which pushed
        // this object onto the stack. This is used to delete the instruction
        // which pushed this object if the optimizer is able to determine that
        // the object itself is not needed.
        //
        // As an alternative to holing a valid offset, `instr_offset` can also
        // take on the following sentinel values:
        //  * UNKNOWN_INSTR_OFFSET, if the object is known to be on the stack
        //    but we don't know which instruction it is associated with.
        //  * GHOST_INSTR_OFFSET, if the object is a ghost object which is not
        //    computed by any instruction. Ghost objects are used when the
        //    optimizer lacks enough static information to generate the code
        //    which computes an object, but it nonetheless has some information
        //    about properties of the object. The optimizer can push a ghost
        //    SymbolicObject onto the stack which represents the known
        //    properties, and downstream optimizations can use those properties
        //    to optimize around uses of the object. If the downstream
        //    optimizations are eventually able to determine that the object is
        //    not needed and delete it, then the ghost object disappears at
        //    compile time and we have succesfully generated optimized code. If
        //    not, we cannot perform the original optimization that generated
        //    the ghost object, and we revert to locally unoptimized code.

    union {
        struct {
            struct SymbolicObject *receiver;
                // If this object was pushed on the stack by a SEND or CALL
                // instruction, this is the symbolic representation of the
                // receiver object, which was popped from the stack by that
                // instruction.
            struct SymbolicObject *message;
                // If this object was pushed on the stack by a SEND, SENDTO,
                // CALL, or CALLTO instruction, this is the symbolic
                // representation of the message object, which was popped from
                // the stack by that instruction.
        };

        struct SymbolicObject *captures;
            // If this object was pushed on the stack by a BLOCKI or CLOSEI
            // instruction, this is the symbolic representation of the top first
            // message object (outermost capture) which was popped from the
            // stack by that instruction.
    };
    struct SymbolicObject *next;
        // If this object is in a list of messages captured by a BLOCKI or
        // CLOSEI instruction (headed by the `captures` field of some other
        // SymbolicObject) then this is the next object in the list.

    // If this object is the result of a statically determinable, referentially
    // transparent expression, it's value is described by `value_type` and
    // `value`. References to the object may be replaced by any set of
    // instructions which compute an equivalent value; such replacements will
    // not change the meaning of the program.
    //
    // The kinds of referentially transparent values recognized by the optimizer
    // are:
    //  * VALUE_SYMBOL:
    //      a symbol object, created by a SYMI instruction. Since symbols have
    //      no internal state, one symbol object may always be substituted for
    //      another symbol object with the same name.
    //  * VALUE_LAMBDA:
    //      a block object, created by a BLOCKI or CLOSEI instruction, where the
    //      optimizer has statically determined that the object's code will
    //      never send a message to any symbols in the object's own scope. Since
    //      the object thus has no scope, and so no internal state, the program
    //      cannot tell if a different object instance with the same code is
    //      substituted in
    //      its place.
    //  * VALUE_OBJECT:
    //      a specific object instance, created by an OBJI instruction. Any
    //      two instruction sequences which result in the same Object pointer
    //      are very clearly equivalent in value.
    //
    enum {
        VALUE_SYMBOL,
        VALUE_LAMBDA,
        VALUE_OBJECT,
        VALUE_UNKNOWN,
    } value_type;
    union {
        const Symbol *symbol;
        Object *object;
        struct {
            // Fields from the BLOCKI or CLOSEI instruction.
            const Symbol *msg_name;
            Bytecode *code;
            BlockFlags flags;
            size_t specialized;
            ScopedObject *scope;

            struct SymbolicObject *captures;
                // A NULL-terminated linked list containing all of the objects
                // which should be captured in the scope of this lambda, which
                // are not captured by the parent object of the lambda's scope.
                // This includes, from outermost to innermost, the message which
                // the parent object is processing when the lambda is created
                // and all the messages being processed by objects which were
                // inlined in that scope.
        } lambda;
    } value;
} SymbolicObject;

static inline void SymbolicObject_CopyValue(
    const SymbolicObject *src, SymbolicObject *dst)
{
    dst->value_type = src->value_type;
    dst->value = src->value;
}

SPECIALIZE_RESULT_STACK(SymbolicObjectStack, SymbolicObject);

////////////////////////////////////////////////////////////////////////////////
// Symbolically evaluated call stacks
//
// Just as the optimizer maintains a symbolic representation of the result stack
// generated by code it is optimizing, it also creates a symbolic representation
// of a portion of the call stack.
//
// Most stack frames are actually not represented explicitly by the optimizer;
// instead it references their runtime values implicitly by generating
// instructions which reference information stored on the call stack. For
// example, the BLOCKI instruction implicitly creates a new object as a child of
// the `scope` field of the current stack frame.
//
// However, when the optimizer inlines a procedure, it can't generate
// instructions which implicitly reference the stack like this, because at
// runtime there will be no stack frame corresponding to the inlined procedure
// call. Instead, the optimizer must generate instructions which encode
// explicitly the information which would have been represented implicitly on
// the stack. For example, the CLOSEI instruction can be used in place of a
// BLOCKI instruction to explicitly name the parent of the newly created object.
//
// In order to accomplish this, the optimizer maintains a stack of
// SymbolicFrames which represents statically known information about the would-
// be stack frames for each procedure call which is currently being inlined.
//

typedef struct SymbolicFrame {
    struct SymbolicFrame *up;
        // The parent frame (that is, the frame corresopnding to the "caller" of
        // this inlined procedure). This will be `NULL` if this is the outermost
        // procedure currently being inlined, in which case information about
        // the caller's stack frame can be found on the actual call stack at
        // runtime.
    ScopedObject *scope;
        // If the procedure being inlined corresponds to a specific, known
        // object, this is that object. This may be `NULL` if the procedure
        // corresponds to an anonymous object whose creation has been elided by
        // the optimizer.
    SymbolicObject *arg;
        // The argument to the procedure call being inlined.
    SymbolicObject *captures;
        // Messages captured by the parent object.
    BlockFlags flags;
        // Object creation flags describing the object whose code is being
        // inlined.
    bool tail_call;
        // Is the call being inlined in a tail position in the enclosing out-of-
        // line procedure?
    bool use_result;
        // Should the ultimate result of the inlined call appear on the result
        // stack?
} SymbolicFrame;

////////////////////////////////////////////////////////////////////////////////
// Global optimizer state
//
// State which is global throughout a single optimization pass is stored in the
// Optimizer data structure. There is one of these per Blimp. It is initialized
// once and then partially deinitialized and reinitialize before each
// optimization pass.
//
// There is also a convenient high-level interface for common optimizer
// operations defined in terms of the Optimizer struct, so callers don't have to
// worry about the individual pieces of state containe within.
//

typedef struct {
    SymbolicObjectStack result_stack;
        // Stack of objects resulting from previous symbolically executed
        // instructions.
    SymbolicFrame *stack;
        // Inlined call stack.
    DeBruijnMap messages;
        // Messages captured by the object whose code we're optimizing.
    Bytecode *replace_subroutine;
        // Subroutine to replace with a more optimized version if we see any
        // references to it.
    Bytecode *optimized_subroutine;
        // Optimized version of `replace_subroutine`.
    size_t specialized;
        // Sequence number of the scope in which the newly optimized code will
        // be specialized.
    CheckpointAllocator sym_objects;
        // Allocator for SymbolicObjects.
    Bytecode *code;
        // The optimized code (appended to as we go).
    size_t ghost_objects;
        // The number of pending ghost objects which the optimizer has not yet
        // managed to delete. If there are still outstanding ghost objects when
        // we reach a checkpoint, we must fail and backtrack, undoing
        // optimizations if necessary, to prevent those ghost objects from being
        // created.
} Optimizer;

typedef struct {
    size_t code_offset;
    size_t stack_size;
    size_t ghost_objects;
    AllocatorCheckpoint sym_objects_checkpoint;
} OptimizerCheckpoint;

/**
 * \brief Perform one-time Optimizer initialization.
 */
PRIVATE Status Optimizer_Init(Blimp *blimp, Optimizer *optimizer);

/**
 * \brief Destroy persisten Optimizer resources.
 *
 * This is the opposite of Optimizer_Init().
 */
PRIVATE void Optimizer_Destroy(Optimizer *optimizer);

/**
 * \brief
 *      Perform per-pass initialization before starting a new optimization pass.
 *
 * \param scope
 *      A scope containing the object whose code is being optimized. This is
 *      required, but need not be the object itself (the object may not even
 *      exist yet when the code is optimized!); it can be any object which is
 *      guaranteed to be a parent or ancestor of the eventual object itself,
 *      including the global object.
 * \param depth
 *      The lexical depth from `scope` to the actual object whose code is being
 *      optimized. This should be 0 if the code to be optimized belongs to
 *      `scope` itself, 1 if the code belongs to a direct child of `scope`, and
 *      so on.
 * \param expr
 *      The expression which originally resulted in the code to be optimized.
 *      This expression will be associated with the optimized bytecode for
 *      debugging purposes.
 * \param replace_subroutine
 *      A subroutine which may be referenced in the code to be optimized. If
 *      non-NULL, all calls to `replace_subroutine` in the code being optimized
 *      will be replaced with calls to `optimized_subroutine` in the optimized
 *      code.
 * \param optimized_subroutine
 *      An optimized version of `replace_subroutine`.
 * \param specialized
 *      The sequence number of the scope in which the newly optimized code will
 *      be specialized.
 */
PRIVATE Status Optimizer_Begin(
    Optimizer *optimizer,
    ScopedObject *scope,
    size_t depth,
    Expr *expr,
    Bytecode *replace_subroutine,
    Bytecode *optimized_subroutine,
    size_t specialized);

/**
 * \brief Clean up after an optmization pass.
 *
 * \param[out] code The optimized code generated during the pass.
 *
 * Calls to this function must be paired with calls to Optimizer_Begin(), and
 * pairs of calls may not be nested.
 */
PRIVATE void Optimizer_End(Optimizer *opt, Bytecode **code);

/**
 * \brief
 *      Save a checkpoint which can be returned to later if an optimization
 *      fails.
 *
 * The state saved by Optimizer_SaveCheckpoint() and restored by
 * Optimizer_RestoreCheckpoint() includes
 *  * the code generated by the optimization pass at the time
 *    Optimizer_SaveCheckpoint() is called
 *  * the result stack
 *  * the number of outstanding ghost objects
 */
PRIVATE OptimizerCheckpoint Optimizer_SaveCheckpoint(Optimizer *opt);

/**
 * \brief Restore optimizer state saved by a call to Optimizer_SaveCheckpoint().
 */
PRIVATE void Optimizer_RestoreCheckpoint(
    Optimizer *opt, OptimizerCheckpoint checkpoint);

/**
 * \brief
 *      Assert that all ghost objects created since `checkpoint` have been
 *      deleted.
 *
 * If not, an error is returned.
 */
PRIVATE Status Optimizer_GhostCheck(
    Optimizer *opt, OptimizerCheckpoint checkpoint);

/**
 * \brief Append an instruction to the optimized code and produce a result.
 *
 * A copy of the instruction `instr` will be appended to the optimized bytecode
 * procedure currently being generated by the optimizer. In addition, if the
 * result type of `instr` is `RESULT_USE`, or if it is `RESULT_INHERIT` and the
 * instruction is in a tail position, then a new SymbolicObject will be pushed
 * onto the optimizer's result stack to represent the result of executing the
 * instruction.
 *
 * The SymbolicObject created to represent the new result will have value type
 * `VALUE_UNKNOWN`; that is, the optimizer currently knows nothing about it, and
 * this function will not inspect the contents of `instr` to try to determine
 * its value. That is left to the caller, so the new object, in addition to
 * being pushed onto the stack, is returned in `result`. If no new object is
 * created, then `*result` is `NULL` after this function returns.
 *
 * If the optimizer is currently in an inlined procedure (`opt->stack` is
 * non-NULL) and the result type of `instr` is `RESULT_INHERIT` and the
 * instruction is not in a tail position in the overall procedure, then the
 * result type of the appended instruction will be changed to `RESULT_USE` or
 * `RESULT_IGNORE`, depending on the value of `use_result` in the current
 * `SymbolicFrame`.
 */
PRIVATE Status Optimizer_Emit(
    Optimizer *opt, const Instruction *instr, SymbolicObject **result);

/**
 * \brief Produce a result which will not be generated at runtime.
 *
 * Similar to Optimizer_Emit(), this function creates a new SymbolicObject with
 * value type `VALUE_UNKNOWN` and pushes it onto the result stack. Unlike
 * Optimizer_Emit(), though, the new object is not produced by any instruction.
 * Instead, its `instr_offset` is set to `GHOST_INSTR_OFFSET`, and it must be
 * deleted by later optimizations before the optimization pass can succeed.
 */
PRIVATE Status Optimizer_EmitGhost(
    Optimizer *opt, ResultType result_type, SymbolicObject **result);

/**
 * \brief Delete the code which generated a result object.
 *
 * This function informs the optimizer that `obj` (which must have come from the
 * result stack) will no longer be needed at runtime.
 *
 * If `obj` is a real object, then the code which generated it will be deleted
 * (or changed to `RESULT_INHERIT` if it has side-effects which must be
 * preserved) so that `obj` is never pushed on the result stack in the first
 * place when the code is executed at runtime. The code deletion is recursive:
 * if it results in the deletion of an instruction with inputs on the result
 * stack, those inputs are deleted, possibly resulting in the deletion of more
 * code.
 *
 * If `obj` is a ghost object, then the count of outstanding ghost objects is
 * decremented, and the optimization which created the ghost object is allowed
 * to remain in effect.
 *
 * This function does not actually delete the SymbolicObject data structure
 * itself, or resources associated with it. It merely removes its association
 * with the optimized program. The object itself can continue to be used until
 * Optimizer_End() is called, or until Optimizer_RestoreCheckpoint() is called
 * with a checkpoint which was saved before the creation of the object.
 */
PRIVATE void Optimizer_Delete(Optimizer *opt, SymbolicObject *obj);

/**
 * \brief Get a captured message in the scope being optimized.
 */
PRIVATE SymbolicObject *Optimizer_GetMessage(Optimizer *opt, size_t index);

/**
 * \brief Try to replace a subroutine with an optimized version.
 *
 * \param [in,out] to_replace
 *      If `*to_replace` matches the `replace_subroutine` passed to the last
 *      call to Optimizer_Begin(), then after this function returns
 *      `*to_replace` is the value of `optimized_subroutine` which was passed to
 *      Optimizer_Begin(). Otherwise, `*to_replace` is unchanged.
 * \param[in,out] specialized
 *      The sequence number of the scope in which `*to_replace` is specialized.
 *      If `*to_replace` is updated by this function, then `*specialized` is
 *      also updated accordingly.
 *
 * The caller must own a reference to `*to_replace`. If `*to_replace` is updated
 * by this function, then reference counts are also updated accordingly. That
 * is, the caller's reference to the old value of `*to_replace` is released and
 * when this function returns the caller owns a reference to the new value of
 * `*to_replace`.
 */
PRIVATE void Optimizer_ReplaceSubroutine(
    Optimizer *opt, Bytecode **to_replace, size_t *specialized);


/**
 * \brief Get the Blimp associated with an Optimizer.
 */
static inline Blimp *Optimizer_Blimp(Optimizer *opt)
{
    return opt->messages.blimp;
}


/**
 * \brief Should the result of an instruction be pushed onto the result stack?
 */
static inline bool Optimizer_UseResult(Optimizer *opt, ResultType result_type)
{
    switch (result_type) {
        case RESULT_USE: return true;
        case RESULT_INHERIT: return !opt->stack || opt->stack->use_result;
            // Instructions with result type `RESULT_INHERIT` are the last
            // instruction in their procedure, and their result is the overall
            // result of the procedure. For these instructions, we produce a
            // result if they are the last instruction in the overall procedure
            // (which is the case if we're not inlining) or if the procedure
            // being inlined is supposed to produce a result (which is indicated
            // by the `use_result` field of the stack frame).
        default: return false;
    }
}

/**
 * \brief Begin processing an inlined procedure call.
 *
 * The SymbolicFrame `frame` is pushed onto the stack. `frame` is not copied, so
 * the memory containing `frame` must remaing valid until the corresponding call
 * to Optimizer_InlineReturn().
 *
 * Calls to this function must be paired with Optimizer_InlineReturn(). Pairs
 * of calls can be nested.
 */
static inline void Optimizer_InlineCall(Optimizer *opt, SymbolicFrame *frame)
{
    frame->up = opt->stack;
    opt->stack = frame;
}

/**
 * \brief End processing of an inlined procedure call.
 *
 * The SymbolicFrame which was pushed onto the stack by the corresponding call
 * to Optimizer_InlineCall() is removed from the stack.
 */
static inline void Optimizer_InlineReturn(Optimizer *opt)
{
    assert(opt->stack != NULL);
    opt->stack = opt->stack->up;
}

/**
 * \brief Remove the top SymbolicObject from the result stack and return it.
 */
static inline SymbolicObject *Optimizer_Pop(Optimizer *opt)
{
    return SymbolicObjectStack_Pop(
        Optimizer_Blimp(opt), &opt->result_stack);
}

#endif
