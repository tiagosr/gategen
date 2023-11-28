#lang s-exp gategen

(gateware counter (width)
    (sig o)
    (sig v width #:reset (- (pow 2 width) 1))
    
    (@ 'sync (connect v (+ v 1)))
    (@       (connect o (pick v -1))))