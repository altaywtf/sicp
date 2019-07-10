#lang sicp

(define tolerance 0.001)

(define (square x) (* x x))

(define (good-enough? guess x)
  (<
   (abs (- (improve guess x) guess))
   (* tolerance guess)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(sqrt-iter 5123123 10000000000)