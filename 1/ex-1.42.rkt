#lang sicp

; ex-1.42: implement compose function, x |-> f((g(x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x) (* x x))
(define (inc x) (+ x 1))

((compose square inc) 6) ; -> 49
((compose inc square) 6) ; -> 37
