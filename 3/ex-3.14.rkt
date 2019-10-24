#lang sicp

; ex-3.14: mystery :o

(define (mystery input)
  (define (loop x y)
    #|
    (newline)
    (display "input: ")
    (display input)
    (newline)
    (display "x: ")
    (display x)
    (newline)
    (display "y: ")
    (display y)
    (newline)
   |#
    
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))

  (loop input '()))

; mystery reverses the list

(define v (list 'a 'b 'c 'd))
v ; -> (a b c d)

(define w (mystery v))
w ; -> (d c b a)
v ; -> (a) -> was expecting () but it's actually modified in the first iteration (debug above)
