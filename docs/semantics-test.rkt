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
        (eval foo M (symbol x r))
        x
    )
    '(foo)
)

; Eval a block literal
(check-equal?
    (judgment-holds
        (eval (block a b) M (block x e r))
        (x e)
    )
    '((a b))
)


; Check a sequence of expressions.
(check-equal?
    (judgment-holds
        (eval (seq foo (block a b)) M (block x e r))
        (x e)
    )
    '((a b))
)

; Test basic virtual dispatch:
;   bind c m foo;
;   {c|.} {m|.}
; Should evaluate to
;   foo
(check-equal?
    (judgment-holds
        (eval (seq (bind c m foo) ((block c |.|) (block m |.|))) M (symbol x r))
        x
    )
    '(foo)
)

; Test more complicated dispatch, where we have to evaluate the operands to get their classes.
;   bind c m foo;
;   (.; {c|.}) (.; {m|.})
; Should evaluate to
;   foo
(check-equal?
    (judgment-holds
        (eval (seq (bind c m foo) ((seq |.| (block c |.|)) (seq |.| (block m |.|)))) M (symbol x r))
        x
    )
    '(foo)
)

; Test rebinding.
;   bind c m foo;
;   bind c m bar;
;   {c|.} {m|.}
; Should evaluate to
;   bar
(check-equal?
    (judgment-holds
        (eval (seq (bind c m foo) (seq (bind c m bar) ((block c |.|) (block m |.|)))) M (symbol x r))
        x
    )
    '(bar)
)

; Test dynamic binding, where we have to evaluate the operands of `bind`.
;   bind (.; c) (.; m) (.; foo);
;   {c|.} {m|.}
; Should evaluate to
;   foo
(check-equal?
    (judgment-holds
        (eval (seq (bind (seq |.| c) (seq |.| m) (seq |.| foo)) ((block c |.|) (block m |.|))) M (symbol x r))
        x
    )
    '(foo)
)

; Test primitive get and set
;   foo{:=|bar};
;   foo{.get|.}
; Should evaluate to
;   bar
(check-equal?
    (judgment-holds
        (eval (seq (foo (block |{:=}| bar)) (foo (block |{.get}| |.|))) M (symbol x r))
        x)
    '(bar)
)

; Test reset
;   foo{:=|bar};
;   foo{:=|baz};
;   foo{.get|.}
; Should evaluate to
;   baz
(check-equal?
    (judgment-holds
        (eval (seq (seq (foo (block |{:=}| bar)) (foo (block |{:=}| baz))) (foo (block |{.get}| |.|))) M (symbol x r))
        x)
    '(baz)
)

; Test primitive eval
;   {do|foo}{.eval|.}
; Should evaluate to
;   foo
(check-equal?
    (judgment-holds
        (eval ((block do foo) (block |{.eval}| |.|)) M (symbol x r))
        x)
    '(foo)
)

; Test get from inner scope
;   x{:=|0};
;   {do|x{.get|.}}{.eval|.}
; Should evaluate to
;   0
(check-equal?
    (judgment-holds
        (eval (seq (x (block |{:=}| |0|)) ((block do (x (block |{.get}| |.|))) (block |{.eval}| |.|))) M (symbol x r))
        x)
    '(|0|)
)

)