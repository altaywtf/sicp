#lang sicp

; ex-2.32: find subsets of a set

; map
(define (map fn items)
  (if (null? items) nil
      (cons (fn (car items)) (map fn (cdr items)))))


; subset finder
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))


; test
(define my-set (list 1 2 3))
(subsets my-set)
