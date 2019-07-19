#lang sicp

(define (square n) (* n n))

; O(√n)
(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? divisor n)
  (= 0 (remainder n divisor)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))))

; Solution
(smallest-divisor 199)   ; -> 199
(smallest-divisor 1999)  ; -> 1999
(smallest-divisor 19999) ; -> 7
