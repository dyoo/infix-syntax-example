#lang racket

;; This provides a structure definition for infix-syntax-transformer
;; that we only use at compile-time.
(require (for-syntax "infix-syntax-compile-time.rkt"
                     syntax/boundmap))

(provide my-app)

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
