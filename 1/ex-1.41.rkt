#lang sicp

; ex-1.41: define a procedure 'double' which applies the original procedure twice

(define (inc x) (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

((double inc) 1) ; -> 1
(((double (double double)) inc) 5) ; -> 21
