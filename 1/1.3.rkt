#lang sicp

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))

(define (ex-1.3 a b c)
  (cond
    ((and (>= a c) (>= b c)) (sum-of-squares a b))
    ((and (>= b a) (>= c a)) (sum-of-squares b c))
    ((and (>= a b) (>= c b)) (sum-of-squares a c))))
  
(ex-1.3 2 1 3)
(ex-1.3 1 2 3)
(ex-1.3 3 2 1)
(ex-1.3 0 0 9)