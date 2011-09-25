#lang s-exp "my-lang.rkt"
(require rackunit
         (for-syntax racket/base))

(define x 42)


(x := (add1 x))
(check-equal? x 43)

(x := (x + 50))
(check-equal? x 93)

;; We can label - as being an infixer as well.
(define-infix-transformer - 
  (lambda (stx) 
    (syntax-case stx () 
      [(lhs op rhs)
       (syntax/loc stx (- lhs rhs))])))


(x := (x - 1))
(check-equal? x 92)

;; This should raise an exception, because the + here masks the one
;; from our language, the one that's been labeled explicitly as
;; an infix operator.
(check-exn
 exn:fail?
 (lambda ()
   (let ([+ (lambda (x y) (+ x y))])
     (x := (x + 50)))))