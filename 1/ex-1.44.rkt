#lang sicp

; ex-1.44: n-fold smoothed function

; from ex-1.43
(define (repeated f times)
  (define (compose f g) (lambda (x) (f (g x))))
  (if (= times 1)
      f
      (compose f (repeated f (- times 1)))))

(define (smooth f)
  (define dx 0.0001)
  (lambda (x)
    (/
     (+ (f (- x dx)) (f x) (f (+ x dx)))
     3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (square x) (* x x))

((n-fold-smooth square 1) 4)
