#lang racket/base

(provide 
    ;(except-out (all-from-out racket) #%module-begin)
    (all-from-out gategen/hdl gategen/lang)
    (rename-out [gategen-module-begin #%module-begin])
    #%app #%datum #%top)

(require gategen/hdl
         gategen/lang
         racket
         syntax/parse/define
         (for-syntax racket/syntax
                     syntax/parse
                     syntax/id-set))

(define-syntax-rule (gategen-module-begin expr)
  (#%module-begin
   (define top-level (gategen #`expr))
   (provide top-level)))

(define-syntax-parser gategen
  [(_ body ...)
   #`(begin
       (compile-gateware body) ...
      )])




(define-for-syntax (gateware form)
  (define-splicing-syntax-class param-spec
    (pattern (~seq (param-id:id default-expr:expr)))
    (pattern (~seq param-id:id)))
  (syntax-parse form
    ((gateware name:id (param:param-spec ...) command:expr ...)
     (define param-id-set (mutable-free-id-set))
     (define signal-id-set (mutable-free-id-set))
     (for-each (lambda (param)
                 (collect-parameters param param-id-set))
               (syntax->list #`(param ...)))
     (for-each (lambda (command)
                 (collect-signal-ids command signal-id-set))
               (syntax->list #`(command ...)))
     #`(begin
         (collect-domains command) ...
         (collect-assignments command) ...
         ))))

(define-for-syntax (collect-parameters item id-set)
  (syntax-parse item
    ((param-id:id default-expr:expr)
     (free-id-set-add! id-set #`param-id))
    (param-id:id
     (free-id-set-add! id-set #`param-id))))

(define-for-syntax (collect-signal-ids item id-set)
  (define-splicing-syntax-class sig-spec
    (pattern (~seq width:integer #:signed))
    (pattern (~seq width:integer #:unsigned))
    (pattern (~seq width:integer)))
  (syntax-parse item
    #:literals (sig like signed unsigned)
    ((sig name:id)
     (free-id-set-add! id-set #`name))
    ((sig name:id spec:sig-spec)
     (free-id-set-add! id-set #`name))
    ((sig name:id (like other:id))
     (free-id-set-add! id-set #`name))
    (_ (void))))

(define-for-syntax (collect-assignments form)
    (syntax-parse form
        #:literals (= =>)
        ((= (lexpr:expr ...) (rexpr:expr ...))
         #`(signal-assign (lexpr ...) (rexpr ...)))
        ((=> (lexpr:expr ...) (rexpr:expr ...))
         #`(signal-assign-latch (lexpr ...) (rexpr ...)))
        (_ (void))))

(define-for-syntax (collect-domains form)
    (syntax-parse form
        #:literals (@ posedge negedge)
        ((@ posedge signal-name:id domain-expr:expr ...)
         #`(clkdomain-posedge signal-name `(domain-expr ...)))
        ((@ negedge signal-name:id domain-expr:expr ...)
         #`(clkdomain-negedge signal-name `(domain-expr ...)))
        ((@ domain-expr:expr ...)
         #`(combdomain `(domain-expr ...)))
        (_ (void))))

(define-syntax (simulate form)
    (syntax-parse form
        ((simulate description:str process-expr:expr ...)
         #`(define submod-id-set (mutable-free-id-set)))
        ))
