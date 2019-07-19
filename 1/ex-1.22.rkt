#lang sicp

(define (square n) (* n n))

; is-prime? by smallest-divisor implementation O(√n)
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

; Timed-Prime
(define (timed-prime-test n) (start-prime-test n (runtime)))

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
(define (is-even? n) (= 0 (remainder n 2)))

(define (search-for-prime n) (search-for-primes n 3))

(define (search-for-primes n counter)
  (if (> counter 0)
      (if (timed-prime-test n)
          (search-for-primes (+ n 1) (- counter 1))
          (search-for-primes (+ n 1) counter))
      (display "--- \n")))

; Find the three smallest primes larger than 1000, 10000, 100000, 1000000
; (search-for-prime 1000)
; (search-for-prime 10000)
; (search-for-prime 100000)
; (search-for-prime 1000000)
; (search-for-prime 10000000)

(search-for-prime 1000000000)    
(search-for-prime 10000000000) 
(search-for-prime 100000000000)  
(search-for-prime 1000000000000)
(search-for-prime 10000000000000)
