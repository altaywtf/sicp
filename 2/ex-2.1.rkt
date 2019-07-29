#lang sicp

; ex-2.1: enhance make-rat to handle positive and negative arguments

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (make-rat n d)
  (define (gcd a b)
    (if (= b 0) a (gcd b (remainder a b))))
  
  (let ((g (gcd n d)))
    (let ((n (/ n g)) (d (/ d g)))
      (if (< d 0)
          (cons (- n) (abs d))
          (cons n d)))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 9 6))  ; -> 3/2
(print-rat (make-rat -9 6)) ; -> -3/2
(print-rat (make-rat 9 -6)) ; -> -3/2
