#lang racket/base


(provide (struct-out infix-syntax-transformer))
(define-struct infix-syntax-transformer (proc)
  #:property prop:procedure 
  (lambda (struct stx)    
    (raise-syntax-error 
     #f "This infix macro can't be used in non-infix position."
     stx)))

  