#lang s-exp "infix-lang.rkt"
(require rackunit
         (for-syntax racket/base))

(define x 42)


(x := (add1 x))
(check-equal? x 43)

(x := (x + 50))
(check-equal? x 93)

;; We can label - as being an infixer as well.
(declare-infix -)

(x := (x - 1))
(check-equal? x 92)

;; The following should tickle an exception, because when we use the
;; let, the + here masks the one from our language, the one that's
;; been labeled explicitly as an infix operator.
(check-exn
 exn:fail?
 (lambda ()
   (let ([+ (lambda (x y) (+ x y))])
     (x := (x + 50)))))


;; We can declare division to be infix as well:
(declare-infix /)
(check-equal? (3 / 4) (/ 3 4))