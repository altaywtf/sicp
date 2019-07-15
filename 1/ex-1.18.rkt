#lang sicp

; assume that the language has an overwritten * operation which is similar to expt procedure defined before
(define (* a b)
  (if (= b 0) 0
      (+ a (* a (- b 1)))))

; and we also implement double and halve procedures
(define (double a) (+ a a))
(define (halve a) (/ a 2))
(define (even? a) (= (remainder a 2) 0))

; design a new * procedure analogous to fast-expt-iter that uses a logratihmic number of steps
(define (*-i a b)
  (*-iter a b 0))

(define (*-iter a b product)
  (cond ((= b 0)
         product)
        ((even? b)
         (*-iter (double a) (halve b) product))
        (else
         (*-iter a (- b 1) (+ product a)))))

; Test
; (* 101232 3200000) - out of memory xD
(*-i 101232 3200000)
