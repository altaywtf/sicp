#lang sicp

; Linear Transformation
; - http://wiki.drewhess.com/wiki/SICP_exercise_1.19
; - http://www.ysagade.nl/2015/04/25/logarithmic-fib/

(define (even? a) (= (remainder a 2) 0))
(define (square n) (* n n))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter
          a
          b
          (+ (square p) (square q))
          (+ (square q) (* 2 p q))
          (/ count 2)))
        (else
         (fib-iter
          (+ (* b q) (* a q) (* a p))
          (+ (* b p) (* a q))
          p
          q
          (- count 1)))))

(fib 10)