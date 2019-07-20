#lang sicp

; is-prime? with better smallest-divisor implementation O((√n)/2)
; because it doesn't make sense to try dividing by 4 if it's not divisible by 2
(define (square n)
  (* n n))

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


; compare the results with ex-1.22, are they halved? -> yesssssss
(search-for-prime 1000000000000)
; 1000000000039: elapsed time: 66050 -> 44364
; 1000000000061: elapsed time: 64856 -> 38833
; 1000000000063: elapsed time: 79769 -> 38725

(search-for-prime 10000000000000)
; 10000000000037: elapsed time: 210356 -> 132543
; 10000000000051: elapsed time: 200050 -> 117532
; 10000000000099: elapsed time: 224870 -> 136415
