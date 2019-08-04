#lang sicp

; ex-2.25: pick 7 from each list

(define list-a (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr list-a)))))

(define list-b (list (list 7)))
(car (car list-b))

(define list-c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list-c))))))))))))
