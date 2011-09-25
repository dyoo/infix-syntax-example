#lang s-exp "infix-lang.rkt"
(require rackunit
         (for-syntax racket/base))

(define x 42)

;; We can use := in infix position.
(x := (add1 x))
(check-equal? x 43)

;; Furthermore, the infixing wraps around everything that looks like
;; function application, including the use of + here:
(x := (x + 50))
(check-equal? x 93)


;; In our language, we haven't labeled - as an infix operator.
;; But we can label - here as being an infixer:
(declare-infix -)


(x := (x - 1))
(check-equal? x 92)

;; The following should tickle an exception, because when we use
;; the let binding, the + here masks the one from our language,
;; the one that's been labeled explicitly as an infix operator.
(check-exn
 exn:fail?
 (lambda ()
   (let ([+ (lambda (x y) (+ x y))])
     (x := (x + 50)))))


;; We can declare division to be infix as well, as expected.
(declare-infix /)
(check-equal? (3 / 4) (/ 3 4))

(declare-infix ^)
(define ^ expt)
(check-equal? (2 ^ 5) 32)


;; And infixing only kicks into effect when the thing looks
;; like function application, so that the following doesn't get
;; touched.
(check-equal? '(2 ^ 5) (list 2 '^ 5))