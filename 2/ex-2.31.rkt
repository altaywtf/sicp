#lang sicp

; ex-2.31: abstracted map-tree
(define (map-tree fn tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (fn tree))
        (else
         (cons (map-tree fn (car tree))
               (map-tree fn (cdr tree))))))

(define (square-tree tree) (map-tree (lambda (x) (* x x)) tree))

; test
(define tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(square-tree tree)
