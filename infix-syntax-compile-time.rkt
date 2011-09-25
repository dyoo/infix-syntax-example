#lang racket/base
(require syntax/boundmap)


(provide (struct-out infix-syntax-transformer)
         auxiliary-infix-transformers)


(define-struct infix-syntax-transformer (proc)
  #:property prop:procedure 
  (lambda (struct stx)    
    (raise-syntax-error 
     #f "This infix macro can't be used in non-infix position."
     stx)))


;; If we want to make existing syntactic bindings as
;; infix operators too, we need some way of communicating
;; this out-of-band.  I'll use a hashtable here that
;; exists at compile time.
(define auxiliary-infix-transformers
  (make-free-identifier-mapping))