#lang sicp

; ex-2.54: recursive equal? for list of symbols

(define (equal?? list-1 list-2)
  (or
   (eq? list-1 list-2)
   (and
    (pair? list-1)
    (pair? list-2)
    (equal?? (car list-1) (car list-2))
    (equal?? (cdr list-1) (cdr list-2)))))


(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

(equal?? '(this is a list) '(this is a list))
(equal?? '(this is a list) '(this (is a) list))
