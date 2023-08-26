#lang racket

(provide fsm%
         clock-signal%
         module%
         signal%
         signal-key%)

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
           [named-submodules (make-hash)]
           [named-wires (make-hash)]
           [wires (make-hash)])
    (define/public (new-signal name width signed? local?)
      (let ([sig (new signal% [name name] [width width] [signed? signed?])])
        (hash-set! wires name sig)
        (unless local?
          (hash-set! named-wires name sig))
        sig))
    (super-new)))

(define signal-key%
    (class object%
        (field [signal (null)]
               [-intern (cond [(is-a? signal signal%) '('signal (get-field signal duid))]
                              [(is-a? signal clock-signal%) '('clock (get-field signal domain))])])))