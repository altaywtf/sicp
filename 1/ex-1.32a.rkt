#lang sicp

; ex-1.32a: write a recursive procedure "accumulate" which generates a recursive process
; use it to re-create sum and product procedures
; -> (accumulate combiner null-value term a next b)

; helpers
(define (inc x) (+ x 1))
(define (identity x) x)


; recursive accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate combiner null-value term (next a) next b))))


; sum
(define (sum a b)
  (accumulate + 0 identity a inc b))

(sum 1 10) ; -> 55


; product
(define (product a b)
  (accumulate * 1 identity a inc b))

(product 1 4) ; -> 24
