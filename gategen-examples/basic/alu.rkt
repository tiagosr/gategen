#lang s-exp gategen

(gateware alu (width)
  (sig sel 2)
  (sig a width #:signed)
  (sig b width #:signed)
  (sig o width #:signed)
  (sig co)
  
  (@ (cond [(eq? sel #b00) (connect o (or a b))]
           [(eq? sel #b01) (connect o (and a b))]
           [(eq? sel #b10) (connect o (xor a b))]
           [(eq? sel #b11) (connect [concat o co] (- a b))])))