#lang sicp

; ex-1.33: write a filtered accumulate, takes a predictate.
; b: calculate the product of all positive integers less than n that are relatively prime to n -> gcd(i, n) = 1

; helpers
(define (inc x) (+ x 1))
(define (identity x) x)


; filtered-accumulate
(define (filtered-accumulate combiner null-value term a next b predictate)
  (if (> a b)
      null-value
      (combiner
       (if (predictate a)
           (term a)
           null-value)
       (filtered-accumulate combiner null-value term (next a) next b predictate))))


; Euclid's Algorithm from ch-1.2.5
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

(define (product-of-relatively-primes n)
  (define (is-relatively-prime? i) (= 1 (gcd i n)))
  (filtered-accumulate * 1 identity 1 inc n is-relatively-prime?))

(product-of-relatively-primes 6)  ; -> 5
(product-of-relatively-primes 7)  ; -> 420
(product-of-relatively-primes 10) ; -> 189
