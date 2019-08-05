#lang sicp

; ex-2.21: plain + map implementations of (square-list (list 1 2 3 4)

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))


(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))

(define (square-list-map items)
  (map square items))

(square-list-map (list 1 2 3 4))
