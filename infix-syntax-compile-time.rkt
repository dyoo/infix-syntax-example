#lang racket/base
(require syntax/boundmap)

(provide auxiliary-infix-transformers)

;; If we want to make existing syntactic bindings as
;; infix operators too, we need some way of communicating
;; this out-of-band.  I'll use a hashtable here that
;; exists at compile time.
(define auxiliary-infix-transformers
  (make-free-identifier-mapping))