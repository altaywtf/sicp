#lang sicp

; ex-2.34: implement horner's rule with acc
; -> https://en.wikipedia.org/wiki/Horner%27s_method


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff sum)
     (+ (* sum x) this-coeff))
   0
   coefficient-sequence))


; 1 + 3x + 5x^3 + x^5 at x = 2
(horner-eval 2 (list 1 3 0 5 0 1))


; 1 + x at x = 2
(horner-eval 2 (list 1 1))
