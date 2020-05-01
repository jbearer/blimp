#lang racket

(require redex)
(require "../../docs/semantics.rkt")
(provide
    test-eval
    test-reduce)

(define-judgment-form bl:mp-machine
    #:mode     (test-nofault I I I)
    #:contract (test-nofault M r e)

    [ (test-step M r e M_2 v)
    --------------------------
      (test-nofault M r e)
    ]
)

(define-metafunction bl:mp-machine
    ; would-fault : M r e -> boolean
    [(would-fault M r e) (not (judgment-holds (test-nofault M r e)))]
)

(define-extended-judgment-form bl:mp-machine steps-to
    #:mode     (test-step I I I O O)
    #:contract (test-step M r e M v)

    [ (test-step M_1 r e_rcv M_2 v_rcv)
            ; first evaluate the receier
      (test-step M_2 r e_msg M_3 v_msg)
            ; and then the message
      (where (block |{!expect}| x_expected r_expected) v_msg)
      (where (symbol x_expected r_rcv) v_rcv)
    --------------------------------------------
      (test-step M_1 r (e_rcv e_msg) M_3 v_rcv)
    ]

    [ (test-step M_1 r e_rcv M_2 (block x_rcv e_body r_rcv))
            ; first evaluate the receier
      (test-step M_2 r e_msg M_3 v_msg)
            ; and then the message
      (where |{!expect{-}error}| (class-of v_msg))
      (side-condition (would-fault M_3 r e_body))
    --------------------------------------------------------------
      (test-step M_1 r (e_rcv e_msg) M_3 v_msg)
    ]
)

(define-judgment-form bl:mp-machine
    #:mode     (test-eval I O O)
    #:contract (test-eval e M v)

    [ (new-machine M r_global)
      (test-step M r_global e M_2 v)
    ---------------------------------
      (test-eval e M_2 v)
    ]
)

(define-judgment-form bl:mp-machine
    #:mode     (test-reduce I O)
    #:contract (test-reduce e v)

    [ (test-eval e M v)
    -----------------------
      (test-reduce e v)
    ]
)
