#lang sicp

; ex-1.35: show that the golden ratio is a fixed point of the transformation x |-> 1 + 1/x

; fixed point procedure from ch-1.3.3
(define (fixed-point f first-guess)
  (define tolerance 0.000001)
  
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))

  (try first-guess))



; in ch-1.2.2 we've seen that phi^2 = phi + 1
; so x^2 = x + 1 is the exact equation which has the root (1+√5)/2 => 1.61803398875

; fixed point for x |-> 1 + 1/x
(define (golden-ratio x)
  (fixed-point
   (lambda (x) (+ 1 (/ 1 x)))
   1.0))

(golden-ratio 2)   ; -> 1.618033813400125
(golden-ratio 100) ; -> 1.618033813400125
