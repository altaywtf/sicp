#lang sicp

; ex-1.33: write a filtered accumulate, takes a predictate.
; a: calculate the sum of the squares of the prime numbers in the interval a to b.

; helpers
(define (inc x) (+ x 1))
(define (identity x) x)
(define (square x) (* x x))


; filtered-accumulate
(define (filtered-accumulate combiner null-value term a next b predictate)
  (if (> a b)
      null-value
      (combiner
       (if (predictate a)
           (term a)
           null-value)
       (filtered-accumulate combiner null-value term (next a) next b predictate))))


; is-prime? implementation from ex-1.23
(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? divisor n)
  (= 0 (remainder n divisor)))

(define (get-next-divisor current)
  (if (= current 2)
      3
      (+ current 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (get-next-divisor test-divisor)))))

(define (is-prime? n)
  (if (= n 1)
      false
      (= n (smallest-divisor n))))


; prime-sum-sqaure
(define (prime-sum-square a b)
  (filtered-accumulate + 0 square a inc b is-prime?))
  
(prime-sum-square 1 5)  ; -> 38
(prime-sum-square 2 14) ; -> 377
