#lang racket/base

(require "infix-syntax.rkt"
         (for-syntax racket/base
                     syntax/boundmap
                     "infix-syntax-compile-time.rkt"))

(provide (except-out (all-from-out racket/base) #%app)
         (rename-out [my-app #%app])
         :=)


;; Here's an example of such an infix macro.
(define-syntax :=
  (infix-syntax-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(lhs _ rhs)
        (syntax/loc stx (set! lhs rhs))]))))


;; The following registers + so that it, too, can be used in infix
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