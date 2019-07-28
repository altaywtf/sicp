#lang sicp

(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))

(define (identity x) x)
(define (cube x) (* x x x))
(define (inc x) (+ x 1))

; sum-integers
(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)

; sum-cubes
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

