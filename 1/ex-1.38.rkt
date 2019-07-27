#lang sicp

; ex-1.38: use cont-frac to approximate e based on Euler's De Fractionibus Continuis
; e -> 2.71828182...

(define (cont-frac-recursive n d k)
  (define (calculate i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (calculate (+ i 1))))))
  
  (calculate 1))


; e-euler
(define (e-euler f k)
  (+ 2
     (f
      (lambda (i) 1.0)
      (lambda (i)
        (cond ((= 0 (remainder i 3)) 1)
              ((= 2 (remainder i 3)) (* (/ (+ i 1) 3) 2))
              ((= 1 (remainder i 3)) 1)))
      k)))

(e-euler cont-frac-recursive 100)
(e-euler cont-frac-recursive 100)
