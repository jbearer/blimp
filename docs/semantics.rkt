#lang racket
(require redex)
(provide eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the semantics of bl:mp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This file defines a semi-formal semantics for the bl:mp language. It defines formally the bl:mp
; language (excluding the concrete syntax and parsing) together with the abstract machine that bl:mp
; programs run in, which includes a few pieces of state not represented syntactically in the
; language. We define semantics for low-level operations in the machine, such as heap loads and
; stores and vtable lookups, and then we use those low-level operations to define `eval`: the
; semantics for how the machine evaluates expressions to produce values.
;
; The semantics here are expressed in Racket using the Redex DSL. If you're not familiar with Redex,
; they have very thorough documentation at docs.racket-lang.org/redex. While Redex provides rich
; facilities for capturing operational semantics, such as reduction relations for expressing
; small-step semantics, we make use of only two:
;  * metafunctions, for helper functions and small, pure, total functions on the machine state
;  * judgment forms, for big step semantics There are two reasons for restricting ourselves to
;    judgment forms instead of reduction relations: 1. I find big-step semantics easier to
;    understand than small-step semantics. 2. Judgment forms, as they are implemented in Redex, are
;    a very natural way to express partial functions: the domain a partial function represented by a
;    judgment form is just the set of input parameters for which the judgment holds, and the
;    codomain is the set of output parameters for all input parameters in the domain. Several
;    operations in bl:mp are defined to cause the abstract machine to "fault". For example, sending
;    a .get message to a symbol which does not exist in the active scope faults the machine. Such
;    operations that might fail can be nicely expressed using judgment forms in this way.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a note on formality
;;
; Above, I referred to this document as a "semi-formal" semantics for bl:mp. What I mean
; "semi-formal" is "formal, with a caveat", the caveat being that the semantics is interpreted by an
; untrusted DSL (Redex) running in an untrusted language (Racket). If we assume that both Redex and
; Racket are completely correct, then this is a completely formal semantics which totally specifies
; the behavior of bl:mp programs. In fact, the `eval` judgment form can easily be used as a bl:mp
; interpreter, albeit a horrifically slow one.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; why bother with formality?
;;
; Mostly because I want to, and because writing Redex semantics is fun. But it is generally useful
; when developing a language to have a clear idea of what the language means. For one thing, it
; serves a useful documentary purpose, since it is more concise and more precise than English prose
; ever can be. (It is also less easy to understand, which is why we should have documentation in
; both forms.)
;
; The formal semantics serves another purpose. By defining the language in total detail in a way
; that matches intuition for the parts of the language which I have thought about and developed
; intuition for, we get a starting point for resolving questions about how parts of the language
; which have not been so carefully designed should behave. That is, if we ever want to know how a
; language construct should work, or how it should interact with another construct, we can ask the
; formal semantics, and it will give us an answer which is guaranteed to be consistent with the rest
; of the language. If we don't like that answer, we can refine the semantics, but it is a good
; starting point.
;
; You might wonder how this is different from just writing an interpreter and defining the language
; based on the behavior of that interpreter. In fact, that's exactly what we're doing, since any
; formal semantics is equivalent to an interpreter (otherwise the language isn't fully specified).
; But there are advantages to defining the semantics separately from the reference implementation of
; the language, and from defining it in this way (with Redex, using the conventions of formal
; language theory):
;   * We can prioritize clarity and stability over performance.
;   * We can provide visibility into the internals of the abstract machine without being tied to the
;     internals of a particular implementation.
;   * We can use the semantics as a reference interpreter to test our practical implementation of an
;     interpreter. For example, we can easily check that a clever new optimization does not result
;     in behavior that visibly deviates from the specification.
;   * We can add optional features and extensions to the interpreter that are not part of the core
;     language, whithout compromising the simplicity of the spec.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; without further ado, the bl:mp lanugage...
;;

(define-language bl:mp
  (e x             ; Symbol literal
     (block e e)   ; Block literal
     (e e)         ; Send
     (bind e e e)  ; Bind
     (seq e e)     ; Sequence
  )

  (x variable-not-otherwise-mentioned)
)

(define-extended-language bl:mp-machine bl:mp
    ; The bl:mp-machine extension to the bl:mp language defines the abstract machine which executes
    ; bl:mp programs. The machine execution state consists of two parts:
    ;   1. The vtable, which maps (receiver class, message class) pairs to methods to execute when a
    ;      message of the message class is sent to an objet of the receiver class. It is modified by
    ;      executing `bind` expressions.
    ;   2. The heap, which maps references to scopes. It is modified whenever an object is created
    ;      (by executing a symbol or object literal) and whenever a scope is modified (see below).
    ;
    ; The job of the abstract machine is to evaluate a given expression, producing a value and a new
    ; machine state. In order to be able to execute all expressions, the machine also defines three
    ; additional instructions:
    ;   1. primitive-get: evaluates to the value of the receiving symbol in the active scope
    ;   2. primitive-set: sets the value of the receiving symbol in the active scope to the result
    ;                     of evaluating the code expression of the message block
    ;   3. primitive-eval: evaluates the code expression of the receiving block
    ; These instructions are accessed like normal methods, via the vtable, which is initialized as
    ; follows:
    ;       receiver class      message class       expression
    ;          symbol               .get           primitive-get
    ;          symbol               .set           primitive-set
    ;          _                    .eval          primitive-eval
    ; They must be accessed via the vtable (as opposed to being statically resolved whenever we see
    ; a send of the appropriate class) because, like all methods, they can be overridden by the user
    ; (although that would probably be unwise).

    ; Values: the final result of evaluating a bl:mp expression.
    (v (block x e r)        ; A block with class:x body:e reference-to-scope:r
       (symbol x r))        ; A symbol object with symbol:x reference-to-scope:r

    ; Scope: map from symbols to values.
    ;
    ; A scope is structured as a log, so the same symbol may have more than one mapping. The most
    ; recent one is valid (that is, contains the value which will be returned by `get`). This makes
    ; updaing a value easier, since we don't have to traverse the scope to do so.
    ;
    ; For example, (a -> 0 : (b -> 0 : (a -> 1 : ε)) is a valid scope, which represents
    ;   a -> 0
    ;   b > 0
    (S r                    ; Reference to parent scope
       ε                    ; Null parent scope (indicates that this scope is global)
       (x -> v : S))        ; A list consisting of one mapping and then a scope, recursively

    ; Heap: map from references to scopes.
    ;
    ; The heap is structured as a log (similar to scopes) terminated by a "fresh referenece"
    ; counter. For example,
    ;   1 -> S_1 : (0 -> S_2 : 2)
    ; represents a heap where the reference `1` points to `S_1`, the reference `0` points to `S_2`,
    ; and `2` is the smallest reference which is "available" to be allocated. The fresh reference
    ; should always be greater than the maximum of all the references in the heap.
    (H r (r -> S : H))
    (r number)              ; References are just numbers


    ; VTable: map from (receiver, message) pairs to methods.
    ;
    ; Like scopes and the heap, the vtable is log-structured to make updates easy.
    (V ∅ (x x -> m : V))

    ; Methods: entries in the vtable.
    ;
    ; A method is either a primitive instruction or a user-defined expression which evaluates when
    ; the method is called.
    (m primitive-get        ; Primitive methods
       primitive-set
       primitive-eval
       e)                   ; Any expression can also be a method.


    (M (H V)) ; The integrated machine
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heap operations
;;

(define-judgment-form bl:mp-machine
    #:mode     (load I I O)
    #:contract (load M r S)
    ; (load M r S) succeeds if `S` is the scope referenced by `r` in the heap of the
    ; machine state `M`.

    ; Check if the first mapping in the heap log is a match.
    [
    -------------------------------
      (load ((r -> S : H) V) r  S)
    ]

    ; If the first mapping doesn't match, check subsequent mappings recursively.
    [ (side-condition (distinct r r_2))
      (load (H V) r S)
    ------------------------------
      (load ((r_2 -> S_2 : H) V) r S)
    ]

)

(define-judgment-form bl:mp-machine
    #:mode     (store I I I O)
    #:contract (store M r S M)
    ; (store M_1 r S M_2) holds if `M_2` is the machine state obtained by storing `S`
    ; into heap location `r` in machine state `M_1.

    [
    -------------------------------------
      (store (H V) r S ((r -> S : H) V))
    ]
)

(define-metafunction bl:mp-machine
    alloc : H -> (H r)
    ; Return a new heap with all of the same entries as the given heap, as well as a reference which
    ; is fresh in the new heap.

    [(alloc r_fresh) (,(+ (term r_fresh) 1) r_fresh)]

    ; Traverse to the end of the heap to find the fresh reference counter.
    [(alloc (r -> S : H)) ((r -> S : H_2) r_fresh)
        (where (H_2 r_fresh) (alloc H))]
)

(define-judgment-form bl:mp-machine
    #:mode     (new I I O O)
    #:contract (new M S M r)
    ; (new M_1 S M_2 r) holds if `M_2` is the machine state obtained by allocating a new reference
    ; `r` in `M_1` and storing the scope `S` there.

    [ (where (H_2 r) (alloc H))
      (store (H_2 V) r S M_3)
    ----------------------------------------
      (new (H V) S M_3 r)
    ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scope operations
;;

(define-judgment-form bl:mp-machine
    #:mode     (scope-lookup I I O)
    #:contract (scope-lookup S x v)
    ; (scope-lookup S x v) holds if `v` is the current value of `x` in `S`.
    ;
    ; This judgment will never hold if `x` does not exist in `S`; that is, the bl:mp machine faults
    ; if it tries to look up a reference in a scope where it doesn't exist.
    ;
    ; This mechanism does not consider parent scopes; it is a simple map lookup in a single,
    ; isolated scope. For a lookup operation that tries to find the key in the parent scope if it is
    ; not present in the original scope, use `get`.

    [
    ----------------------------------
      (scope-lookup (x -> v : S) x v)
    ]

    [ (scope-lookup S x v)
      (side-condition (distinct x x_2))
    --------------------------------------
      (scope-lookup (x_2 -> v_2 : S) x v)
    ]
)

(define-judgment-form bl:mp-machine
    #:mode     (scope-parent I O)
    #:contract (scope-parent S r)
    ; (scope-parent S r) holds if the scope `S` has a parent scope (that is, it is not the global
    ; scope) and if `r` is a reference to that scope.

    [
    ---------------------
      (scope-parent r r)
    ]

    [ (scope-parent S r)
    -------------------------------
      (scope-parent (x -> v : S) r)
    ]
)

(define-judgment-form bl:mp-machine
    #:mode     (get I I I O)
    #:contract (get M r x v)
    ; (get M r x v) holds if `v` is the current value of `x` in the scope pointed to by the
    ; reference `r` in the machine state `M`.
    ;
    ; If the symbol `x` is not found in the scope referred to by `r`, but `r` has a parent scope,
    ; this mechanism will recursively try to `get` the value of `x` from the parent scope. If `x` is
    ; not found in the scope `r` or any of its parents, the machine faults.

    ; Try looking up `x` in the current scope.
    [ (load M r S)
      (scope-lookup S x v)
    -----------------------
      (get M r x v)
    ]

    ; If that fails, get the parent of the current scope and get the value of `x` from there.
    [ (load M r S)
      (scope-parent S r_parent)
      (get M r_parent x v)
    -----------------------
      (get M r x v)
    ]
)

(define-judgment-form bl:mp-machine
    #:mode     (set I I I I O)
    #:contract (set M r x v M)
    ; (set M_1 r x v M_2) holds if `M_2` is the machine state obtained by setting the value of `x`
    ; to `v` in the scope referred to by `r` in the initial machine state `M_1`.
    ;
    ; If `x` exists in the current scope or any parent of the current scope, it's value will be set
    ; in the scope closest to the current scope where it exists. If `x` does not exist in any scope,
    ; it will be added to the current scope and it's value will be set there.

    ; If the symbol exists in this scope, update it in this scope.
    [ (load M r S)
      (scope-lookup S x v_old)
      (store M r (x -> v : S) M_2)
    ----------------------------
      (set M r x v M_2)
    ]

    ; If it exists in a parent scope, store it there.
    [ (load M r S)
      (scope-parent S r_parent)
      (get M r_parent x v_old)
      (set M r_parent x v M_2)
    ---------------------------
      (set M r x v M_2)
    ]

    ; Otherwise, add it to the current scope.
    [ (load M r S)
      (store M r (x -> v : S) M_2)
    -------------------------------
      (set M r x v M_2)
    ]
)

(define-metafunction bl:mp-machine
    value-scope : v -> r
    ; Get a reference to the scope associated with a value.
    [(value-scope (symbol x r))  r]
    [(value-scope (block x e r)) r]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vtable operations
;;

(define-metafunction bl:mp-machine
    bind-method : M x x m -> M
    ; Update the machine state `M` to store a new method binding `m` associated with the given
    ; receiver and message symbols. The result is the new machine state.
    [(bind-method (H V) x_rcv x_msg m) (H (x_rcv x_msg -> m : V))]
)

(define-metafunction bl:mp-machine
    class-of : v -> x
    ; Get the symbol for the class of a value. For symbols, this is always `symbol`. For blocks, it
    ; is the tag of the block.
    [(class-of (symbol x r)) sym]
    [(class-of (block x e r)) x]
)

(define-metafunction bl:mp-machine
    ; Check if the symbol pair (receiver class, message class) exists in the given vtable.
    [(vtable-contains (x_rcv  x_msg  -> m : V) x_rcv x_msg) #t]
    [(vtable-contains (x_rcv2 x_msg2 -> m : V) x_rcv x_msg) (vtable-contains V x_rcv x_msg)]
    [(vtable-contains ∅                        x_rcv x_msg) #f]
)

(define-judgment-form bl:mp-machine
    #:mode     (resolves-to I I I O)
    #:contract (resolves-to M x x m)
    ; (resolves-to M x_rcv x_msg m) holds if `m` is the method that should be evaluate when a
    ; message of class `x_msg` is sent to a receiver of class `x_rcv`.

    ; Check the first mapping in the vtable.
    [
    ---------------------------------------------------------
      (resolves-to (H (x_rcv x_msg -> m : V)) x_rcv x_msg m)
    ]

    ; Check remaining mappings recursively.
    [ (resolves-to (H V) x_rcv x_msg m)
      (side-condition (distinct (x_rcv x_msg) (x_rcv2 x_msg2)))
    -------------------------------------------------------------
      (resolves-to (H (x_rcv2 x_msg2 -> m_2 : V)) x_rcv x_msg m)
    ]

    ; If we didn't find (x_rcv, x_msg), look for (x_rcv, _) to check if the receiver has a fall-
    ; through handler to process messages dynamically.
    [ (side-condition (distinct x_msg |{under}|))
      (side-condition (¬(vtable-contains V x_rcv x_msg)))
      (resolves-to (H V) x_rcv |{under}| m)
    ----------------------------------------------------------
      (resolves-to (H V) x_rcv x_msg m)
    ]

    ; If that failed, look for (_, x_msg) to see if there is a global handler for this message.
    [ (side-condition (distinct x_rcv |{under}|))
      (side-condition (¬(vtable-contains V x_rcv x_msg)))
      (side-condition (¬(vtable-contains V x_rcv |{under}|)))
      (resolves-to (H V) |{under}| x_msg m)
    ----------------------------------------------------------
      (resolves-to (H V) x_rcv x_msg m)
    ]
)

(define-judgment-form bl:mp-machine
    #:mode     (call-method I I I I I O O)
    #:contract (call-method M r m v v M v)
    ; (call-method M_1 r m v_rcv v_msg M_2 v_m) holds if, given initial machine state `M_1`, the
    ; method `m` with receiver `v_rcv` and messge `v_msg` evaluates to `v_m` in the scope `r`, and
    ; the new machine state is `M_2`.

    ; For primitive-get, we don't call anything, we just look up the symbol directly.
    [ (get M r x_rcv v)
    -----------------------------------------------------------------
      (call-method M r primitive-get (symbol x_rcv r_rcv) v_msg M v)
    ]

    ; For primitive-set, we first evaluate the body of the message, and then we store the resulting
    ; value in the current scope.
    [ (steps-to M_1 r_msg e_msg M_2 v_msg)
      (where (symbol x_rcv r_rcv) v_rcv)
      (set M_2 r x_rcv v_msg M_3)
    -------------------------------------------------------------------------------
      (call-method M_1 r primitive-set v_rcv (block x_msg e_msg r_msg) M_3 v_rcv)
    ]

    ; For primitive-eval, just evaluate the body of the receiver.
    [ (steps-to M_1 r_rcv e_rcv M_2 v_rcv)
    -------------------------------------------------------------------------------
      (call-method M_1 r primitive-eval (block x_rcv e_rcv r_rcv) v_msg M_2 v_rcv)
    ]

    ; For user-defined methods, evaluate the expression in the context of the receiver.
    [
      ; Store the receiver object in `this` and the message in `that` so that the method can access
      ; them.
      (set M_1 (value-scope v_rcv) this v_rcv M_2)
      (set M_2 (value-scope v_rcv) that v_msg M_3)

      ; Evaluate the body.
      (steps-to M_3 (value-scope v_rcv) e_mth M_4 v_mth)
    -----------------------------------------------------
      (call-method M_1 r e_mth v_rcv v_msg M_4 v_mth)
    ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine execution
;;

(define-judgment-form bl:mp-machine
    #:mode     (steps-to I I I O O)
    #:contract (steps-to M r e M v)
    ; (steps-to M_1 r e M_2 v) holds if `e` evaluates to `v` in the scope `r` given initial machine
    ; state `M_1`, and the new state is `M_2`.

    ; A symbol is a primitive: just allocate a new object with the given symbol.
    [
      (new M_1 r_parent M_2 r_scope)
    -----------------------------------------------------
      (steps-to M_1 r_parent x M_2 (symbol x r_scope))
    ]

    ; A block steps by evaluating its class tag, which must evaluate to a symbol. We do not evaluate
    ; the expression in the body of the block; that will only be evaluated when the block receives a
    ; `.eval` message.
    [ (steps-to M_1 r_parent e_cls                   M_2 (symbol x_cls r_cls))
      (new M_2 r_parent M_3 r_scope)
    --------------------------------------------------------------------------
      (steps-to M_1 r_parent (block e_cls e_bod)     M_3 (block x_cls e_bod r_scope))
    ]

    ; To evaluate a message send,
    [ (steps-to M_1 r e_rcv M_2 v_rcv)
            ; first evaluate the receier
      (steps-to M_2 r e_msg M_3 v_msg)
            ; and then the message
      (resolves-to M_3 (class-of v_rcv) (class-of v_msg) m_bod)
            ; then look up the method in the vtable. It must exist, otherwise the overall judgment
            ; does not hold, and the abstract machine faults. If it does exist, and we get back the
            ; expression `e_bod`, then the evaluation of the overal send is the evaluation of
            ; `e_bod` in the scope of `v_rcv`.
      (call-method M_3 r m_bod v_rcv v_msg M_4 v_bod)
            ; Evaluate the method
    -------------------------------------------------------------------------
      (steps-to M_1 r (e_rcv e_msg) M_4 v_bod)
    ]

    ; To evaluate a bind,
    [ (steps-to M_1 r e_rcv                       M_2 v_rcv)
      (where (symbol x_rcv r_rcv) v_rcv)
            ; first evaluate the receiver class (it must evaluate to a symbol)
      (steps-to M_2 r e_msg                       M_3 (symbol x_msg r_msg))
            ; then evaluate the message class (it must evaluate to a symbol)
     ; We do not evaluate the expression being bound; it will be evaluated each time the method is
     ; called (see the evaluation rule for `send` above).
    ---------------------------------------------------------------------------------------
      (steps-to M_1 r (bind e_rcv e_msg e_bod)    (bind-method M_3 x_rcv x_msg e_bod) v_rcv)
                                                    ; Insert the new mapping into the vtable
    ]

    ; A `seq` evaluates the first expression only for its effects on the environment, and then
    ; evaluates the second expression for its result.
    [ (steps-to M_1 r e_1                   M_2 v_1 )
      (steps-to M_2 r e_2                   M_3 v_2 )
    ----------------------------------------------------------
      (steps-to M_1 r (seq e_1 e_2)         M_3 v_2 )
    ]
)

(define-judgment-form bl:mp-machine
    #:mode     (eval I O O)
    #:contract (eval e M v)
    ; (eval e M v) holds if `e` evaluates to `v` in the global scope with the default machine state,
    ; and the resulting machine state is `M`.
    ;
    ; This is the top-level entrypoint for evaluating complete bl:mp programs.

    [ ;; Initialize the vtable
      (where V ( sym    .get  -> primitive-get
               :(sym    :=    -> primitive-set
               :(|{under}|  .eval -> primitive-eval
               : ∅
               ))))

      ;; Initialize the heap containing the global scope
      (new (0 V) ε M r_global)

      ;; Evaluate in the global scope
      (steps-to M r_global e M_2 v)
    ----------------------------------
      (eval e M_2 v)
    ]

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;

(define-metafunction bl:mp
    [(distinct any   any)   #f]
    [(distinct any_1 any_2) #t]
)
