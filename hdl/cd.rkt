#lang racket

(provide clock-domain%)
(require gategen/hdl/ast
         racket/syntax)

(define clock-domain%
    (let ([signal-name-for (lambda (domain-name signal-name)
            (if (eq? 'sync domain-name)
                signal-name
                (format-symbol "~a-~a" domain-name signal-name))) ]
          [cd-remover (lambda (name)
            (if (eq? (substring (symbol->string name) 0 3) "cd_")
                (string->symbol (substring (symbol->string name) 3))
                name))])
        (class object%
            (init [name ""]
                  [reset-less #f])
            (field [(internal-name name) (cd-remover name)]
                   [clk-edge 'pos]
                   [clk (new signal% [name internal-name])]
                   [rst (if (eq? reset-less #t)
                            (void)
                            (new signal% [name (signal-name-for internal-name 'rst)]))]
                   [async-reset #f]
                   [local #f])
            (define (rename new-name)
                (begin
                    (set! internal-name new-name)
                    (send clk set-name (signal-name-for new-name 'clk))
                    (if (eq? rst (void))
                        (void)
                        (send rst set-name (signal-name-for new-name 'rst)))))
            (super-new))))