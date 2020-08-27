#lang racket
(require redex)
(provide
    bl:mp
    bl:mp-machine

    eval
    new-machine
    steps-to)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the semantics of bl:mp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This file defines a semi-formal semantics for the bl:mp language. It defines formally the bl:mp
; language (excluding the concrete syntax and parsing) together with the abstract machine that bl:mp
; programs run in, which includes a few pieces of state not represented syntactically in the
; language. We define semantics for low-level operations in the machine, such as heap loads and
; stores, and then we use those low-level operations to define `eval`: the semantics for how the
; machine evaluates expressions to produce values.
;
; The semantics here are expressed in Racket using the Redex DSL. If you're not familiar with Redex,
; they have very thorough documentation at docs.racket-lang.org/redex. While Redex provides rich
; facilities for capturing operational semantics, such as reduction relations for expressing
; small-step semantics, we make use of only two:
;  * metafunctions, for helper functions and small, pure, total functions on the machine state
;  * judgment forms, for big step semantics
; There are two reasons for restricting ourselves to judgment forms instead of reduction relations:
;  1. I find big-step semantics easier to understand than small-step semantics.
;  2. Judgment forms, as they are implemented in Redex, are a very natural way to express partial
;     functions: the domain of a partial function represented by a judgment form is just the set of
;     input parameters for which the judgment holds, and the codomain is the set of output
;     parameters for all input parameters in the domain. Some operations in bl:mp are defined to
;     cause the abstract machine to "fault". For example, looking up a symbol which does not exist
;     in the active scope faults the machine. Such operations that might fail can be nicely
;     expressed using judgment forms in this way.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a note on formality
;;
; Above, I referred to this document as a "semi-formal" semantics for bl:mp. What I mean by
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
    (e ::= x             ; Symbol literal
           (block x e)   ; Block literal
           (e e)         ; Message send
           (seq e e)     ; Sequence
    )

    (x ::= variable-not-otherwise-mentioned)

    #:binding-forms
    (block x e #:refers-to x)
)

(define-extended-language bl:mp-machine bl:mp
    ; The bl:mp-machine extension to the bl:mp language defines the abstract machine which executes
    ; bl:mp programs. The machine execution state consists of a heap, which is conceptually an
    ; infinite map from references to scopes. It is modified whenever an object is created (by
    ; executing a block literal) and whenever a scope is modified (see below).
    ;
    ; The job of the abstract machine is to evaluate a given expression, producing a value and a new
    ; machine state. In order to be able to execute all expressions, the machine defines an
    ; additional expression non-terminal to represent the primitive behavior of initailizing an
    ; uninitialized symbol by sending it a constructor. The (primitive-set r x e) expression is the
    ; result of sending a message `e` to the reference parameter to a constructor for the symbol `x`
    ; in the scope `r`.

    (e ::= ....
        (value v)
        (primitive-set r x e))

    ; Values: the final result of evaluating a bl:mp expression.
    (v ::=
        x               ; A symbol object
        (obj r x e))    ; A block with reference-to-scope:r parameter:x body:e

    ; Scope: map from symbols to values.
    ;
    ; A scope is structured as a log, so the same symbol may have more than one mapping. The most
    ; recent one is authoritative. This makes updaing a value easier, since we don't have to
    ; traverse the scope to do so.
    ;
    ; For example, (a -> 0 : (b -> 0 : (a -> 1 : ε)) is a valid scope, which represents
    ;   a -> 0
    ;   b -> 0
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

    #:binding-forms
    (obj r x e #:refers-to x)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heap operations
;;

(define-judgment-form bl:mp-machine
    #:mode     (load I I O)
    #:contract (load H r S)
    ; (load H r S) succeeds if `S` is the scope referenced by `r` in the heap `H`.

    ; Check if the first mapping in the heap log is a match.
    [
    -------------------------------
      (load (r -> S : H) r  S)
    ]

    ; If the first mapping doesn't match, check subsequent mappings recursively.
    [ (side-condition (distinct r r_2))
      (load H r S)
    ------------------------------
      (load (r_2 -> S_2 : H) r S)
    ]

)

(define-judgment-form bl:mp-machine
    #:mode     (store I I I O)
    #:contract (store H r S H)
    ; (store H_1 r S H_2) holds if `H_2` is the heap obtained by storing `S` into heap location `r`
    ; in heap `H_1`.

    [
    -------------------------------------
      (store H r S (r -> S : H))
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
    #:contract (new H S H r)
    ; (new H_1 S H_2 r) holds if `H_2` is the heap obtained by allocating a new reference `r` in
    ; `H_1` and storing the scope `S` there.

    [ (where (H_2 r) (alloc H))
      (store H_2 r S H_3)
    ----------------------------------------
      (new H S H_3 r)
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
    #:contract (get H r x v)
    ; (get H r x v) holds if `v` is the current value of `x` in the scope pointed to by the
    ; reference `r` in the heap `H`.
    ;
    ; If the symbol `x` is not found in the scope referred to by `r`, but `r` has a parent scope,
    ; this mechanism will recursively try to `get` the value of `x` from the parent scope. If `x` is
    ; not found in the scope `r` or any of its parents, the machine faults.

    ; Try looking up `x` in the current scope.
    [ (load H r S)
      (scope-lookup S x v)
    -----------------------
      (get H r x v)
    ]

    ; If that fails, get the parent of the current scope and get the value of `x` from there.
    [ (load H r S)
      (scope-parent S r_parent)
      (get H r_parent x v)
    -----------------------
      (get H r x v)
    ]
)

(define-judgment-form bl:mp-machine
    #:mode     (can-get I I I)
    #:contract (can-get H r x)

    [ (get H r x v)
    -------------------
      (can-get H r x)
    ]
)

(define-metafunction bl:mp-machine
    in-scope : H r x -> boolean

    [(in-scope H r x) ,(judgment-holds (can-get H r x))]
)

(define-judgment-form bl:mp-machine
    #:mode     (set I I I I O)
    #:contract (set H r x v H)
    ; (set H_1 r x v H_2) holds if `H_2` is the heap obtained by setting the value of `x` to `v` in
    ; the scope referred to by `r` in the initial heap `H_1`.
    ;
    ; If `x` exists in the current scope or any parent of the current scope, it's value will be set
    ; in the scope closest to the current scope where it exists. If `x` does not exist in any scope,
    ; it will be added to the current scope and it's value will be set there.

    ; If the symbol exists in this scope, update it in this scope.
    [ (load H r S)
      (scope-lookup S x v_old)
      (store H r (x -> v : S) H_2)
    ----------------------------
      (set H r x v H_2)
    ]

    ; If it exists in a parent scope, store it there.
    [ (load H r S)
      (scope-parent S r_parent)
      (side-condition (in-scope H r_parent x))
      (set H r_parent x v H_2)
    ---------------------------
      (set H r x v H_2)
    ]

    ; Otherwise, add it to the current scope.
    [ (side-condition (¬ (in-scope H r x)))
      (load H r S)
      (store H r (x -> v : S) H_2)
    -------------------------------
      (set H r x v H_2)
    ]
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine execution
;;

(define-judgment-form bl:mp-machine
    #:mode     (steps-to I I I O O)
    #:contract (steps-to H r e H v)
    ; (steps-to H_1 r e H_2 v) holds if `e` evaluates to `v` in the scope `r` given initial heap
    ; `H_1`, and the new heap is `H_2`.

    ; A symbol is a primitive: just create a new symbol object.
    [
    -----------------------------------------------------
      (steps-to H r x H x)
    ]

    ; To evaluate a block, allocate a scope for it and return a new object.
    [ (new H_1 r_parent                         H_2 r_scope)
    --------------------------------------------------------------------------
      (steps-to H_1 r_parent (block x e)    H_2 (obj r_scope x e))
    ]

    ; To evaluate a message sent to an object,
    [ (steps-to H_1 r e_rcv H_2 (obj r_scope x e))
            ; first evaluate the receier
      (steps-to H_2 r e_msg H_3 v_msg)
            ; and then the message
      (steps-to H_3 r_scope (substitute e x (value v_msg)) H_4 v_result)
            ; Evaluate the body.
    -------------------------------------------------------------------------
      (steps-to H_1 r (e_rcv e_msg) H_4 v_result)
    ]

    ; To evaluate a message sent to a symbol which is in scope,
    [ (steps-to H_1 r e_rcv H_2 x)
            ; first evaluate the receiver
      (get H_2 r x v)
            ; get the symbol's value
      (steps-to H_2 r ((value v) e_msg) H_3 v_result)
            ; and then send the message to that value
    -------------------------------------------------------------------------
      (steps-to H_1 r (e_rcv e_msg) H_3 v_result)
    ]

    ; To evaluate a message sent to a symbol which is not in scope,
    [ (steps-to H_1 r e_rcv H_2 x)
            ; first evaluate the receier
      (side-condition (¬ (in-scope H_2 r x)))
            ; check that the symbol does not exist
      (steps-to H_2 r (e_msg (block freshvar (primitive-set r x freshvar))) H_3 v_result)
            ; create a reference which can be used to set the value of the symbol, and pass it to
            ; the message, which will act as a constructor for the symbol
    -------------------------------------------------------------------------
      (steps-to H_1 r (e_rcv e_msg) H_3 v_result)
    ]

    [
    ----------------------------------
      (steps-to H r (value v) H v)
    ]

    [
      (set H_1 r_capture x v H_2)
    ------------------------------------------------------------------
      (steps-to H_1 r (primitive-set r_capture x (value v)) H_2 x)
    ]

    ; A `seq` evaluates the first expression only for its effects on the environment, and then
    ; evaluates the second expression for its result.
    [ (steps-to H_1 r e_1                   H_2 v_1 )
      (steps-to H_2 r e_2                   H_3 v_2 )
    ----------------------------------------------------------
      (steps-to H_1 r (seq e_1 e_2)         H_3 v_2 )
    ]
)

(define-judgment-form bl:mp-machine
    #:mode     (new-machine O O)
    #:contract (new-machine H r)

    [
      ;; Initialize the heap containing the global scope
      (new 0 ε H r_global)
    -----------------------------------------------------
      (new-machine H r_global)
    ]
)

(define-judgment-form bl:mp-machine
    #:mode     (eval I O O)
    #:contract (eval e H v)
    ; (eval e H v) holds if `e` evaluates to `v` in the global scope with an empty eap, and the
    ; resulting Heap is `H`.
    ;
    ; This is the top-level entrypoint for evaluating complete bl:mp programs.

    [ (new-machine H r_global)
      (steps-to H r_global e H_2 v)
    ----------------------------------
      (eval e H_2 v)
    ]

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;

(define-metafunction bl:mp
    distinct : any any -> boolean
    [(distinct any   any)   #f]
    [(distinct any_1 any_2) #t]
)

(define-metafunction bl:mp
    ¬ : boolean -> boolean
    [(¬ #t) #f]
    [(¬ #f) #t]
)
