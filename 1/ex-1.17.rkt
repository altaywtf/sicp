#lang sicp

; assume that the language has an overwritten * operation which is similar to expt procedure defined before
(define (* a b)
  (if (= b 0) 0
      (+ a (* a (- b 1)))))

; and we also implement double and halve procedures
(define (double a) (+ a a))
(define (halve a) (/ a 2))
(define (even? a) (= (remainder a 2) 0))

; design a new * procedure analogous to fast-expt that uses a logratihmic number of steps
(define (*-r a b)
  (cond ((= b 0) 0)
        ((even? b) (double (*-r a (halve b))))
        (else (+ a (*-r a (- b 1))))))

; Test
(*-r 4 3)
