#lang sicp

; ch-1.3.4: procedures as returned values

; helpers
(define (square x) (* x x))

; from ch-1.3.3 -> finding fixed points of functions f(x) = x
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


; average damping as transformation
; f(x) -> (x + f(x)) / 2
(define (average-damp f)
  (define (average v1 v2) (/ (+ v1 v2) 2))
  (lambda (x)
    (average x (f x))))

; test with square
((average-damp square) 10) ; -> 55


; derivate as function transformation
; g(x) -> Dg(x) = (g(x + dx) - g(x)) / dx
(define (derivative g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

; test with square
((derivative square) 5) ; -> 10.000009999444615


; newton-transform as function transformation
; g(x) -> x - g(x)/D(g(x)) 
(define (newton-transform g)
  (lambda (x)
    (- x
       (/ (g x) ((derivative g) x)))))


; abstraction for transformations to use in fixed-point
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))


; sqrt with average damping
(define (sqrt-avg-damp x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) ; transformation fn from ch-1.3.3
   average-damp
   1.0))

(sqrt-avg-damp 64) ; -> 8.0


; sqrt with newtons method
(define (sqrt-newton x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) ; transformation fn from newton
   newton-transform
   1.0))

(sqrt-newton 64) ; -> 8.0