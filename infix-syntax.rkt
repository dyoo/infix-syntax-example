#lang racket

;; This provides a structure definition for infix-syntax-transformer
;; that we only use at compile-time.
(require (for-syntax "infix-syntax-compile-time.rkt"))

;; Here's an example of such an infix macro.
(define-syntax :=
  (infix-syntax-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(lhs _ rhs)
        (syntax/loc stx (set! lhs rhs))]))))


;; This defines a protocol for infix syntax that works on top of
;; the function application macro.
;; If we see something like
;;
;;     (lhs op rhs)
;;
;; where op's syntactic value is an infix-syntax-transformer,
;; we let the infix syntax macro take over.
(define-syntax (my-app stx)
  (syntax-case stx ()
    
    [(_ lhs op rhs)
     (and (identifier? #'op)
          (infix-syntax-transformer? (syntax-local-value #'op)))
     ((infix-syntax-transformer-proc (syntax-local-value #'op))
      (syntax/loc stx (lhs op rhs)))]

    ;; Otherwise, just default to regular function application.
    [(_ args ...)
     (syntax/loc stx
       (#%app args ...))]))
    
      