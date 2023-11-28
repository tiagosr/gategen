#lang racket

(provide fsm%
         clock-signal%
         module%
         signal%
         signal-key%
         domain%)

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
           [operands '(state (dict-ref! encoding name (dict-count encoding)))]))
    (public ongoing)
        (super-new)))

(define clock-signal%
  (class signal%
    (field [domain 'sync])
    (super-new [shape 1])))

(define domain%
  (class object%
    (field [name 'sync]
           [edge 'pos]
           [local? #f])
    (super-new)))

(define module%
  (class object%
    (field [ctrl-context '()]
           [ctrl-stack '()]
           [driving (make-hash)]
           [named-submodules (make-hash)]
           [public-wires (make-hash)]
           [wires (make-hash)]
           [domains (make-hash)]
           [current-domain '@]
           [operations '()])
    (define/public (new-signal name width signed? local?)
      (let ([sig (new signal% [name name] [width width] [signed? signed?])])
        (hash-set! wires name sig)
        (unless local?
          (hash-set! public-wires name sig))
        sig))
    (define/public (new-signal-like name other local?)
      (let ([sig (new signal% [name name]
                              [width (send other get-width)]
                              [signed? (send other get-signed?)]
                              [local? local?])])
        (hash-set! wires name sig)
        (unless local?
          (hash-set! public-wires name sig))
        sig))
    (define/public (add-operation op)
      (set! operations (append operations (list op current-domain))))
    (define/public (new-domain name edge local? reset? async-reset?)
      (void))
    
    (super-new)))

(define signal-key%
    (class object%
        (field [signal (null)]
               [-intern (cond [(is-a? signal signal%) '('signal (get-field signal duid))]
                              [(is-a? signal clock-signal%) '('clock (get-field signal domain))])])))