#lang sicp

; ex-2.35 implement count-leaves (from section 2.2.2) of a tree with accumulate

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))


; count-leaves with acc
(define (count-leaves tree)
  (define (identity x) x)
  
  (accumulate
   (lambda (x y)
     (+ y
        (if (pair? x) (count-leaves x) 1)))
   0
   (map identity tree)))


; test 
(define x (cons (list 1 2) (list 3 4)))
(count-leaves x) ; 4
(count-leaves (list x x)) ; 8
(count-leaves (cons nil (list 1 2))) ; 3
