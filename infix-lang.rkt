#lang racket/base

(require (for-syntax racket/base
                     syntax/boundmap))

(provide (except-out (all-from-out racket/base) #%app)
         (rename-out [my-app #%app])
         
         define-infix-transformer
         +
         :=)


(begin-for-syntax
  ;; If we want to make existing syntactic bindings as
  ;; infix operators too, we need some way of communicating
  ;; this out-of-band.  I'll use a hashtable here that
  ;; exists at compile time.
  (define auxiliary-infix-transformers
    (make-free-identifier-mapping)))


;; This defines a protocol for infix syntax that works on top of
;; the function application macro.
;; If we see something like
;;
;;     (lhs op rhs)
;;
;; where op's syntactic value is an infix operator, then we let
;; the infix syntax macro take over.
(define-syntax (my-app stx)
  (syntax-case stx ()
    ;; If the operator has been labeled as infix, by being in
    ;; auxiliary-infix-transformers, then the infix protocol activates:
    [(_ lhs op rhs)
     (and (identifier? #'op)
          (free-identifier-mapping-get auxiliary-infix-transformers
                                       #'op
                                       (lambda () #f)))
     ((free-identifier-mapping-get auxiliary-infix-transformers #'op)
      (syntax/loc stx (lhs op rhs)))]
    
    ;; Otherwise, just default to regular function application.
    [(_ args ...)
     (syntax/loc stx
       (#%app args ...))]))





;; The following registers + so that it can be used in infix
;; position.  The code is a bit ugly; I may want to provide an abstraction
;; to make it nicer to express.
(begin-for-syntax
  (free-identifier-mapping-put! 
   auxiliary-infix-transformers
   #'+
   (lambda (stx)
     (syntax-case stx ()
       [(lhs _ rhs)
        (syntax/loc stx (+ lhs rhs))]))))


;; Ok, let's make the definition of infix operators a little nicer.
;; We'll provide a define-infix form that will do pretty much what
;; we did for the + binding above.
(define-syntax (define-infix-transformer stx)
  (syntax-case stx ()
    [(_ id transformer)
     (syntax/loc stx
       (begin-for-syntax
         (free-identifier-mapping-put! 
          auxiliary-infix-transformers
          #'id
          transformer)))]))


;; Let's define := using define-infix-transformer.
(define-infix-transformer :=
  (lambda (stx)
    (syntax-case stx ()
      [(lhs _ rhs)
       (syntax/loc stx (set! lhs rhs))])))


(define-syntax (:= stx)
  (raise-syntax-error #f "Can't be used in non-infix position" stx))