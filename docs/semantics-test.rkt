#lang racket

(require rackunit)
(require redex)
(require "semantics.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brief sanity check for bl:mp semantics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Run with `raco test semantics-test.rkt`.
; For a more thorough test, use the bl:mp test suite (blimp-test).
;

(module+ test

; Eval a symbol literal
(check-equal?
    (judgment-holds
        (eval foo H v)
        v
    )
    '(foo)
)

; Eval a block literal
(check-equal?
    (judgment-holds
        (eval (block a b) H (obj r x e))
        e
    )
    '(b)
)

; Check a sequence of expressions.
(check-equal?
    (judgment-holds
        (eval (seq foo (block a b)) H (obj r x e))
        e
    )
    '(b)
)

; Check a trivial message send
(check-equal?
    (judgment-holds
        (eval ((block a a) foo) H x)
        x)
    '(foo)
)

; Check defining a symbol
(check-equal?
    (judgment-holds
        (eval (seq (foo (block ref (ref (block a a))))
                   (foo bar))
            H x)
        x)
    '(bar)
)

; Check accessing a symbol in a nested scope
(check-equal?
    (judgment-holds
        (eval (seq (foo (block ref (ref (block a a))))
                   ((block a (a bar)) foo))
            H x)
        x)
    '(bar)
)

; Check setting a symbol
(check-equal?
    (judgment-holds
        (eval (seq (foo (block ref (ref ref)))
              (seq (foo (block a bar))
                   (foo get)))
            H x)
        x)
    '(bar)
)

; Check setting a symbol in a nested scope
(check-equal?
    (judgment-holds
        (eval (seq (foo (block ref (ref ref)))
              (seq ((block a (foo (block a bar))) z)
                   (foo get)))
            H x)
        x)
    '(bar)
)

; Check setting a symbol from an unrelated scope
(check-equal?
    (judgment-holds
        (eval (seq (scope (block scoperef
                        (scoperef (block msg (foo msg)))))
              (seq (scope (block fooref (fooref fooref)))
              (seq (scope (block a a))
                   (scope bar))))
            H x)
        x)
    '(bar)
)

; Check initialize a symbol in both the global scope and a child scope simultaneously.
(check-equal?
    (judgment-holds
        (eval (seq (parent-ref (block ^ (^ ^)))
              (seq (child-ref  (block ^ (^ ^)))
              (seq (foo        (block ^ (parent-ref ^)))
                   ((block -
                        (seq (foo (block ^ (child-ref ^)))
                        (seq (parent-ref (block ^ foo))
                        (seq (child-ref  (block ^ bar))
                             (foo -))))
                    )-))))
            H x)
        x)
    '(bar)
)

)
