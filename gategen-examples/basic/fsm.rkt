#lang s-exp gategen

(gateware uart-rx (divisor)
    (sig i)
    (sig data width)
    (sig rdy)
    (sig ack)
    (sig err)

    (sig ctr (bits-fit divisor) #:local)
    (sig stb #:local)
    (if (= ctr 0)
        [(@sync (connect ctr (- divisor 1)))
         (@     (connect stb 1))]
        [(@sync (connect ctr (- ctr 1)))])
        
    (sig bit 3 #:local)
    (fsm
        (state 'start
            (unless i
                [(@sync (connect ctr (>> divisor 1))
                        (connect bit 7))
                 (next-state 'data)]))
        (state 'data
            (if stb
                [(@sync (connect bit (- bit 1))
                        (connect data (concat i data)))
                 (if (= bit 0)
                     [(next-state 'stop)])]))
        (state 'stop
            (if stb
                [(if i
                     [(next-state 'done)]
                     [(next-state 'error)])]))
        (state 'done
            (@ (connect rdy 1))
            (if ack
                [(next-state 'start)]))
        (@ (connect err (= ongoing-state 'error)))
        (state 'error)))