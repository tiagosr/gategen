#lang s-exp gategen

(gateware blinky ()
    (sig timer 20)
    (platform-sig led ('led 0))
    
    (@ 'sync [= timer (+ timer 1)])
    (@       [= led ($ timer -1)]))