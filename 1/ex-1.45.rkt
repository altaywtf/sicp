#lang racket

; ex-1.45: repeated average damp to find fixed-points for y |-> x/y^(n-1)

; from ch-1.3
(define (average-damp f)
  (define (average v1 v2) (/ (+ v1 v2) 2))
  (lambda (x) (average x (f x))))


; from ch-1.3.3
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


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))


; from ex-1.43
(define (repeated f times)
  (define (compose f g) (lambda (x) (f (g x))))
  (if (= times 1)
      f
      (compose f (repeated f (- times 1)))))


; repeatedly-average-damped-fixed-point
(define (repeatedly-average-damped-fixed-point f times)
  (fixed-point-of-transform
   f
   (repeated average-damp times)
   1.0
   ))


; sqrt
(define (sqrt x)
  (repeatedly-average-damped-fixed-point
   (lambda (y) (/ x y))
   1))

(sqrt 4) ; -> 2.000000000000002

; cubic-root
(define (cubic-root x)
  (define (square x) (* x x))
  (repeatedly-average-damped-fixed-point
   (lambda (y) (/ x (square y)))
   2))

(cubic-root 8) ; -> 2.0000001352154335
