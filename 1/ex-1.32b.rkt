#lang sicp

; ex-1.32b: write a recursive procedure "accumulate" which generates an iterative process
; use it to re-create sum and product procedures
; -> (accumulate combiner null-value term a next b)

; helpers
(define (inc x) (+ x 1))
(define (identity x) x)


; iterative accumulate
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))

  (iter a null-value))


; sum
(define (sum a b)
  (accumulate + 0 identity a inc b))

(sum 1 10) ; -> 55


; product
(define (product a b)
  (accumulate * 1 identity a inc b))

(product 1 4) ; -> 24

