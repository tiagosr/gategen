#lang s-exp gategen

(gateware adder (width)
    (sig a width #:signed)
    (sig b width #:signed)
    (sig o (+ width 1) #:signed)

    (@ [connect o (+ a b)]))

(gateware subtractor (width)
    (sig a width #:signed)
    (sig b width #:signed)
    (sig o (+ width 1) #:signed)
    
    (@ [connect o (- a b)]))

(gateware alu (width)
    (sig op)
    (sig a width #:signed)
    (sig b width #:signed)
    (sig o (+ width 1) #:signed)

    (adder add [width width])
    (subtractor sub [width width])

    (@
        [connect add.a a]
        [connect add.b b]
        [connect sub.a a]
        [connect sub.b b]
        (if (op)
            ([connect o sub.o])
            ([connect o add.o]))))