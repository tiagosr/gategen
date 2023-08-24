#lang s-exp gategen

(gateware adder (width)
    (sig a width #:signed)
    (sig b width #:signed)
    (sig o (+ width 1) #:signed)

    (@ [= o (+ a b)]))

(gateware subtractor (width)
    (sig a width #:signed)
    (sig b width #:signed)
    (sig o (+ width 1) #:signed)
    
    (@ [= o (- a b)]))

(gateware alu (width)
    (sig op)
    (sig a width #:signed)
    (sig b width #:signed)
    (sig o (+ width 1) #:signed)

    (adder add [width width])
    (subtractor sub [width width])

    (@
        [= add.a a]
        [= add.b b]
        [= sub.a a]
        [= sub.b b]
        (if (op)
            ([= o sub.o])
            ([= o add.o]))))