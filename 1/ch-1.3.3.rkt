#lang sicp

; ch-1.3.3 -> Finding fixed points of functions f(x) = x
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


(fixed-point cos 1.0) ; 0.7390855263619245

(fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0) ; 1.2587277968014188

; sqrt implementation that uses y |-> 1/2(y+x/y) transformation because y |-> x/y doesn't converge well
(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point
   (lambda (y) (average y (/ x y)))
   1.0))

(sqrt 4) ; 2.000000000000002
