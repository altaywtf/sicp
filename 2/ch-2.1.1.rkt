#lang sicp

; make-rat utility to represent our rational numbers as cons (car, cdr) pairs

(define (basic-make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))


; enhanced make-rat that converts 6/8 to 3/4 by using gcd
(define (make-rat n d)
  ; from ch-1.2.5 - euclid's algorithm
  (define (gcd a b)
    (if (= b 0) a
        (gcd b (remainder a b))))
  
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 9 6)) ; -> 3/2
(print-rat (make-rat 123182 21312)) ; -> 61591/10656
