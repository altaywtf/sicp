#lang sicp

; ex-1.36: find a solution for x^x = 1000 by applying x |-> log(1000)/log(x)
; don't use 1.0 as guess because log(1) = 0 (:d)

(define (report current next step-number)
  (display step-number)
  (newline)
  (display "current guess ")
  (display current)
  (newline)
  (display "next guess ")
  (display next)
  (newline)
  (display "---")
  (newline))

; fixed point procedure from ch-1.3.3
(define (fixed-point f first-guess)
  (define tolerance 0.000001)
  
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try guess step-number)
    (let ((next (f guess)))
      (report guess next step-number)
      
      (if (close-enough? guess next)
          next
          (try next (+ step-number 1)))))

  (try first-guess 1))


; fixed point for x |-> log(1000) / log(x)
(fixed-point
 (lambda (x) (/ (log 1000) (log x))) 2.0)
; result -> 4.555535422664798
; #-step -> 40


; fixed point for x |-> ((log(1000) / log(x)) + x)/2 (average damped)
(fixed-point
 (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0)
; result -> 4.555535758730802
; #-step -> 11
