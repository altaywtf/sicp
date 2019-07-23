#lang sicp

; ex-1.30: rewrite sum as iterative process

; helpers
(define (inc x) (+ x 1))
(define (identity x) x)


; recursive sum higher-order-procedure
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))


; iterative sum
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


; sum-integers
(define (sum-integers a b) (sum identity a inc b))
(sum-integers 1 100)
; -> 5050

(define (sum-integers-iter a b) (sum-iter identity a inc b))
(sum-integers-iter 1 100)
; -> 5050
