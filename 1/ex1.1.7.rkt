#lang sicp

(define tolerance 0.001)

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) tolerance))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(sqrt-iter 2 9)