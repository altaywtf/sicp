#lang sicp

; ex-1.46: iterative-improve procedure
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter guess)
      (if (good-enough? guess)
          guess
          (iter (improve guess))))

    (iter guess)))


; helpers
(define (square x) (* x x))
(define (average v1 v2) (/ (+ v1 v2) 2))


; rewriting sqrt from ch-1.1.7
(define (sqrt x)
  (define tolerance 0.001)
  
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))
  
  (define (improve guess)
    (average guess (/ x guess)))

  ((iterative-improve good-enough? improve) 1.0))

(sqrt 16) ; -> 4.000000636692939


; rewriting fixed-point from ch-1.3.3
(define (fixed-point f guess)
  (define tolerance 0.000001)
  
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))

  (define (improve guess)
    (f guess))

  ((iterative-improve good-enough? improve) guess))


(fixed-point cos 1.0) ; 0.7390845495752126
