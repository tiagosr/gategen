#lang racket

(provide duid% signal% operator% value% as-signed as-unsigned)

(define duid%
  (let ([current-id 0])
    (class object%
      (field (id current-id))
      (set! current-id (+ current-id 1))
      (super-new))))

(define signal%
  (class object%
    (init-field (name 'unnamed)
                (width 1)
                (signed? #f))
    (define (set-name new-name)
      (set! name new-name))
    (define (as-unsigned)
      (new operator% [operator 'u]
           [operands '((this))]))
    (define (as-signed)
      (new operator% [operator 's]
           [operands '((this))]))
    (define (boolean)
      (new operator% [operator 'b]
           [operands '((this))]))
    (define (any)
      (new operator% [operator 'any]
           [operands '((this))]))
    (define (all)
      (new operator% [operator 'all]
           [operands '((this))]))
    (define (xor)
      (new operator% [operator 'xor]
           [operands '((this))]))
    (public set-name as-unsigned as-signed
            boolean any all xor)
    (super-new)))

(define operator%
  (class object%
    (init-field (operator 'u)
                (operands '()))
    (super-new)))

(define value%
  (class object%
    (field (src_loc '()))
    (super-new)))

(define (as-signed sig)
  (new operator% [operator 'u] [operands (list sig)]))

(define (as-unsigned sig)
  (new operator% [operator 'u] [operands (list sig)]))