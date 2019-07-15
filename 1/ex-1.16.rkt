#lang sicp

(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

; Successsive Squaring
; - even -> b^n = b^(n/2) * b^(n/2)
; - odd  -> b^n = b * b^(n-1)

; Recursive Fast-Expt
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))

(fast-expt 2 3)
(fast-expt 2 16)
(fast-expt 100 100)

; Iterative Fast-Expt
; Hint
; - Use b^n = (b^(n/2)) ^ 2 = (b^2) ^ (n/2)
; - Use three state variables: b, n and a
; - Define the state transfirmation in such a way a(b^n) is unchanged state to state (invariant quantity)
; - a starts as 1 and the result of the process is latest value of a
(define (fast-expt-i b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n)
         (fast-expt-iter (square b) (/ n 2) a))
        (else
         (fast-expt-iter b (- n 1) (* a b)))))

(fast-expt-i 2 3)
(fast-expt-i 2 16)
(fast-expt-i 100 100)