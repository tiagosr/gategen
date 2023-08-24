#lang racket

(provide (all-defined-out))

(require gategen/hdl/ast
    racket/dict)

(define fsm%
    (class object%
        (field [state 'init]
               [encoding (make-hash)]
               [decoding (make-hash)])
        (define (ongoing name)
            (new operator% 
                 [operation '==]
                 [operands '(state 
                             (dict-ref! encoding name (dict-count encoding)))]))
        (public ongoing)
        (super-new)))

(define clock-signal%
    (class signal%
        (field [domain 'sync])
        (super-new [shape 1])))

(define module%
    (class object%
        (field [ctrl-context (null)]
               [ctrl-stack '()]
               [driving (make-hash)]
               [named-submodules (make-hash)])
        (super-new)))

(define signal-key%
    (class object%
        (field [signal (null)]
               [-intern (cond [(is-a? signal signal%) '('signal (get-field signal duid))]
                              [(is-a? signal clock-signal%) '('clock (get-field signal domain))])])))