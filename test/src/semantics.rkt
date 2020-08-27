#lang racket

(require redex)
(require "../../docs/semantics.rkt")
(provide
    test-bl:mp-machine
    test-eval
    test-reduce)

(define-extended-language test-bl:mp-machine bl:mp-machine
  (e ::= ....
         (primitive-expect-eq e e)
         (|{!}| x)
  )
)

(define-judgment-form test-bl:mp-machine
    #:mode     (test-nofault I I I)
    #:contract (test-nofault H r e)

    [ (test-step H r e H_2 v)
    --------------------------
      (test-nofault H r e)
    ]
)

(define-metafunction test-bl:mp-machine
    would-fault : H r e -> boolean
    [(would-fault H r e) ,(not (judgment-holds (test-nofault H r e)))]
)

(define-extended-judgment-form test-bl:mp-machine steps-to
    #:mode     (test-step I I I O O)
    #:contract (test-step H r e H v)

    [ (test-step H_1 r (block ^a (block ^b (primitive-expect-eq ^a ^b))) H_2 v)
    ---------------------------------------------------
      (test-step H_1 r (|{!}| |{expect{-}eq}|) H_2 v)
    ]

    [
      (test-step H_1 r e_1 H_2 x)
      (test-step H_2 r e_2 H_3 x)
    ------------------------------------------------------------
      (test-step H_1 r (primitive-expect-eq e_1 e_2) H_3 |{.}|)
    ]
)

(define-judgment-form test-bl:mp-machine
    #:mode     (test-eval I O O)
    #:contract (test-eval e H v)

    [ (new-machine H r_global)
      (test-step H r_global e H_2 v)
    ---------------------------------
      (test-eval e H_2 v)
    ]
)

(define-judgment-form test-bl:mp-machine
    #:mode     (test-reduce I O)
    #:contract (test-reduce e v)

    [ (test-eval e H v)
    -----------------------
      (test-reduce e v)
    ]
)
