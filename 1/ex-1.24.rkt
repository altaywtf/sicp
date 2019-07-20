#lang sicp

; use fast-prime with fermat test - O(logn)
(define (square x)
  (* x x))

(define (is-even? x)
  (= 0 (remainder x 2)))

; fermat's Little Theorem -> if modulo of a^n is equal to a, n is probably not a prime number.
(define (expmod base exponent modulo)
  (cond ((= 0 exponent) 1)
        ((is-even? exponent)
         (remainder (square (expmod base (/ exponent 2) modulo)) modulo))
        (else
         (remainder (* base (expmod base (- exponent 1) modulo)) modulo))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define random-number
    (+ 1 (random (- n 1))))
  (try-it random-number))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


; timed-prime-test
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display ": elapsed time: ")
  (display elapsed-time)
  (newline))

; search-for-prime -> checks the primality of consecutive odd integers in a specified range
(define (search-for-prime n)
  (search-for-primes n 3))

(define (search-for-primes n counter)
  (if (> counter 0)
      (if (timed-prime-test n)
          (search-for-primes (+ n 1) (- counter 1))
          (search-for-primes (+ n 1) counter))
      (display "--- \n")))


; compare the growth (log10 -> 1) -> seems to be correct (~1.2 growth rate)
(search-for-prime 1000)
; 1009: elapsed time: 221
; 1013: elapsed time: 223
; 1019: elapsed time: 213

(search-for-prime 10000)
; 10007: elapsed time: 259
; 10009: elapsed time: 252
; 10037: elapsed time: 255

(search-for-prime 100000)
; 100003: elapsed time: 303
; 100019: elapsed time: 305
; 100043: elapsed time: 306

(search-for-prime 1000000)
; 1000003: elapsed time: 387
; 1000033: elapsed time: 413
; 1000037: elapsed time: 351

; couldn't test with the bigger numbers like in ex-1.22 and 1.23 because the random procedure throws:
; (random 10000000000) -> expected: (or/c (integer-in 1 4294967087) pseudo-random-generator?)
