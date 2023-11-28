#lang racket/base

(provide 
    ;(except-out (all-from-out racket) #%module-begin)
    (all-from-out gategen/hdl/dsl gategen/lang)
    (rename-out [gategen-module-begin #%module-begin])
    #%app #%datum #%top
    gateware
    sig
    platform-sig)

(require gategen/hdl/dsl
         gategen/lang
         racket
         syntax/parse/define
         syntax/wrap-modbeg
         (for-syntax racket/syntax
                     syntax/parse
                     syntax/id-set
                     syntax/parse/debug
                     macro-debugger/emit))

(define-syntaxes (sig platform-sig) (values #f #f))
(define-syntaxes (@ posedge negedge) (values #f #f #f))

(define-syntax gategen-module-begin (make-wrapping-module-begin #'gategen))

(define-syntax (gategen body)
  (syntax-parse body
    #:literals (gateware)
    [(_ (gateware name:id body ...))
     #`(println (syntax->datum name))]
    [(_ form)
     #`(println form)]
    [_ #`(begin)]))


(define-syntax (gateware form)
  (define-splicing-syntax-class param-spec
    (pattern (~or (~seq (param-id:id default-expr:expr))
                  (~seq param-id:id))))
  (syntax-parse form
    ((_ name:id (param:id ...) command:expr ...)
     (define param-id-set (mutable-free-id-set))
     (define signal-id-set (mutable-free-id-set))
     (let ([root-items (map (lambda (command)
                           (collect-signals command signal-id-set))
                         (syntax->list #`(command ...)))])
       #`(let ([this-module (new module%)])
           (begin
             (define (name param ...)
               (let ([instantiated (send this-module instantiate name param ...)])
                 #,@root-items
                 instantiated))
             name))))))

(define-for-syntax (collect-parameters item id-set)
  (syntax-parse item
    ((param-id:id default-expr:expr)
     (free-id-set-add! id-set #`param-id))
    (param-id:id
     (free-id-set-add! id-set #`param-id))))

(define-for-syntax (collect-signals form id-set)
  (syntax-parse form
    #:literals (sig platform-sig)
    ((sig name:id (~optional (~and has-local? #:local)))
     #:with maybe-local (if (attribute has-local?) #`#t #`#f)
     #`(define name (send this-module new-signal 'name 1 #f maybe-local)))
    ((sig name:id sig-width
          (~alt
           (~optional (~or (~and has-signed? #:signed)
                           (~and has-unsigned? #:unsigned)))
           (~optional (~and has-local? #:local))) ...)
     #:with maybe-local (if (attribute has-local?) #`#t #`#f)
     #:with maybe-signed (if (attribute has-signed?) #`#t #`#f)
     #`(define name (send this-module new-signal 'name sig-width maybe-signed maybe-local)))
    ((sig name:id #:like other:id (~optional (~and has-local? #:local)))
     #:with maybe-local (if (attribute has-local?) #`#t #`#f)
     (free-id-set-add! id-set #`name))
    ((platform-sig name:id ('plat-name:id))
     (free-id-set-add! id-set #`name))
    ((platform-sig name:id ('plat-name:id index:number))
     (free-id-set-add! id-set #`name))
    (_ (collect-domains form))))

(define-for-syntax (collect-conditionals-l0 form)
  (syntax-parse form
    #:literals (cond if and or = eq? ~=)
    ((if conditional:expr if-true:expr if-false:expr)
     #`(send this-module add-if-statement #`conditional #`if-true #`if-false))
    ((if conditional:expr if-true:expr)
     #`(send this-module add-if-statement #`conditional #`if-true '()))
    (_ (collect-domains form))))

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
    ((@ posedge domain-name:id domain-expr:expr ...)
     #`(send this-module clkdomain-posedge 'domain-name (domain-expr ...)))
    ((@ negedge domain-name:id domain-expr:expr ...)
     #`(send this-module clkdomain-negedge 'domain-name (domain-expr ...)))
    ((@ domain-name:id domain-expr:expr ...)
     #`(send this-module clkdomain-posedge 'domain-name (domain-expr ...)))
    ((@ domain-expr:expr ...)
     #`(send this-module combdomain '(domain-expr ...)))))

(define-for-syntax (collect-domain-statements form)
  (syntax-parse form
    #:literals (@ posedge negedge connect fsm)
    ((connect into:id from:expr)
     #`('connect 'into 'from))))

(define-syntax (simulate form)
  (syntax-parse form
    ((simulate description:str process-expr:expr ...)
     #`(define submod-id-set (mutable-free-id-set)))
    ))


