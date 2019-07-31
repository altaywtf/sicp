#lang sicp

; ex-2.5: represent (a, b) pairs as (2^a * 3^b). implement car and cdr accordingly.

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (get-division-count n k)
  (define (divides? k n) (= 0 (remainder n k)))
 
  (if (divides? k n)
      (+ 1 (get-division-count (/ n k) k))
      0))

(define (car n)
  (get-division-count n 2))

(define (cdr n)
  (get-division-count n 3))

; test
(define my-int (cons 5 10))

my-int ; -> 1889568
(car my-int) ; -> 5
(cdr my-int) ; -> 10
