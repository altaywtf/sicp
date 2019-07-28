#lang sicp

; ex-1.40: define a procedure 'cubic' that can be used together with newtons-method
; approximate zeros of the x^3+ax^2+bx+c

; ch-1.3.3 -> fixed-point
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


; ch-1.3.4 -> newton-transform
(define (newton-transform g)
  (define (derivative g)
    (define dx 0.00001)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x)) dx)))
  
  (lambda (x)
    (- x
       (/ (g x) ((derivative g) x)))))


; newtons-method
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


; cubic 
(define (cubic a b c)
  (define (square x) (* x x))
  (define (cube x) (* (square x) x))
  
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))


; (x+1)^3
(newtons-method (cubic 1 3 3) 1) ; -> -0.9999999999999997
