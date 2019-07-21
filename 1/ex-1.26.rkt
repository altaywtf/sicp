#lang sicp

; ex-1.26: inspect expmod implementation that uses explicit squaring
(define (is-even? x)
  (= 0 (remainder x 2)))

; fermat's little theorem -> with explicit squaring
(define (expmod base exponent modulo)
  (cond ((= 0 exponent) 1)
        ((is-even? exponent)
         (remainder
          (* (expmod base (/ exponent 2) modulo)
             (expmod base (/ exponent 2) modulo))
          modulo))
        (else
         (remainder
          (* base (expmod base (- exponent 1) modulo))
          modulo))))

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


#|
- expmod was only calculating the inner part of the square procedure once and now we have two recursive procedures, which results with a tree recursive call-tree.
- since we are using explicit multiplication instead of successive squaring, growth rate changed from o(logn) to o(n)
|#

(search-for-prime 1000)
; 1009: elapsed time: 14877
; 1013: elapsed time: 14568
; 1019: elapsed time: 13839 

(search-for-prime 10000)
; 10007: elapsed time: 183597
; 10009: elapsed time: 170038
; 10037: elapsed time: 185691

(search-for-prime 100000)
; 100003: elapsed time: 1593557
; 100019: elapsed time: 1684407
; 100043: elapsed time: 1691548
