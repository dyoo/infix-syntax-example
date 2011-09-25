#lang racket/base

(require "infix-syntax.rkt"
         (for-syntax racket/base
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
