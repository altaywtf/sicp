#lang sicp

; ex-1.29: use Simpson's Rule to define a more accurate integral procedure and use for cube

; sum higher-order-procedure
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))
(define (inc x) (+ x 1))
(define (is-even? x) (= (remainder x 2) 0))

; for small values of dx, we can use
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (*
   (sum f (+ a (/ dx 2.0)) add-dx b)
   dx))

(integral cube 0 1 0.001)
; -> 0.249999875000001 (actual value 1/4)

(integral cube 5 10 0.001)
; -> 2343.7499906254243


; integral-simpson
(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (* (y k)
       (cond ((or (= k 0) (= k n)) 1)
             ((is-even? k) 2)
             (else 4))))
  (*
   (/ h 3)
   (sum term (y 0) inc n)))


(integral-simpson cube 0 1 100)
; -> 1/4

(integral-simpson cube 5 10 100)
; -> 0 wtf?

(integral-simpson cube 0 1 1000)
; -> 1/4

(integral-simpson cube 5 10 1000)
; -> 2250-ish
