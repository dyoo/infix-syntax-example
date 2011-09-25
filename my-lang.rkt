#lang racket/base

(require "infix-syntax.rkt"
         (for-syntax racket/base
                     syntax/boundmap
                     "infix-syntax-compile-time.rkt"))

(provide (except-out (all-from-out racket/base) #%app)
         (rename-out [my-app #%app])

         define-infix-transformer
         +
         :=)

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



;; Let's make the definition of infix operators a little nicer.
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