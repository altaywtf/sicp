#lang sicp

; Example 1.26 - Prime Number Check

; Helpers
(define (square n) (* n n))
(define (is-even? n) (= 0 (remainder n 2)))

; Method One - O(√n)
(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? divisor n)
  (= 0 (remainder n divisor)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (find-divisor n (+ 1 test-divisor))))

(define (is-prime? n)
  (= (smallest-divisor n) n))

; Method Two: Fermat Test - O(log(n))

; Fermat's Little Theorem -> if modulo of a^n is equal to a, n is probably not a prime number.
(define (expmod base exponent modulo)
  (cond ((= 0 exponent) 1)
        ((is-even? exponent)
         (remainder (square (expmod base (/ exponent 2) modulo)) modulo))
        (else
         (remainder (* base (expmod base (- exponent 1) modulo)) modulo))))

; (define (expmod-2 base exponent modulo) (= (remainder (expt base exponent) modulo) base))

; Fermat Test
(define (fermat-test n)
  (define (try-it a) (= (expmod a n n) a))
  (define random-number (+ 1 (random (- n 1))))
  (try-it random-number))

; Repetetive Fermat Test
(define (fast-prime n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime n (- times 1)))
        (else false)))

(fast-prime 24 3)
