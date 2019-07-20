#lang sicp

; is-prime? with smallest-divisor implementation O(√n)
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? divisor n)
  (= 0 (remainder n divisor)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))))

(define (is-prime? n)
  (= n (smallest-divisor n)))


; timed-prime-test
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (is-prime? n)
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


; find the three smallest primes larger than 1000, 10000, 100000, 1000000 to check time grows with √n (√10 = 3.16)
; (search-for-prime 1000)
; (search-for-prime 10000)
; (search-for-prime 100000)

(search-for-prime 1000000000000)
; 1000000000039: elapsed time: 66050
; 1000000000061: elapsed time: 64856
; 1000000000063: elapsed time: 79769

(search-for-prime 10000000000000)
; 10000000000037: elapsed time: 210356
; 10000000000051: elapsed time: 200050
; 10000000000099: elapsed time: 224870
