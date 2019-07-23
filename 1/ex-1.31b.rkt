#lang sicp

; ex-1.31b: iterative product h-o-p-hophohphohp

; helpers
(define (inc x) (+ x 1))
(define (identity x) x)
(define (is-odd? x) (= 1 (remainder x 2)))


; iterative product h-o-p
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))


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
(pi-approx 1 100)  ; -> 3.1570301764551654
(pi-approx 1 1000) ; -> 3.1431607055322552 -> converges to 3.14 as we increase the number of steps
