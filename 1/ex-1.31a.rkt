#lang sicp

; ex-1.31a: create an higher-order 'product' procedure analogous to sum

; helpers
(define (inc x) (+ x 1))
(define (identity x) x)
(define (is-odd? x) (= 1 (remainder x 2)))


; recursive product h-o-p
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))


; factorial implementation
(define (factorial n) (product identity 1 inc n))
(factorial 5) ; -> 120


; John Wallis's π approximation
(define (pi-approx a number-of-steps)
  (define (term x)
    (if (is-odd? x)
        (/ (+ x 1.0) (+ x 2.0))
        (/ (+ x 2.0) (+ x 1.0))))
  (* 4
     (product term a inc number-of-steps)))

(pi-approx 1 6)    ; -> 3.343673469387755
(pi-approx 1 100)  ; -> 3.1570301764551645
(pi-approx 1 1000) ; -> 3.143160705532257 -> converges to 3.14 as we increase the number of steps
