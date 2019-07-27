#lang sicp

; ex-1.38: use cont-frac to approximate e based on Euler's De Fractionibus Continuis
; e -> 2.71828182...

; from ex-1.37a
(define (cont-frac-recursive n d k)
  (define (calculate i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (calculate (+ i 1))))))
  
  (calculate 1))

; from ex-1.37b
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))

  (iter k 0))


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

(e-euler cont-frac-recursive 100) ; -> 2.7182818284590455
(e-euler cont-frac-iter 100)      ; -> 2.7182818284590455
