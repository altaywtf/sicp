#lang sicp

; ex-2.61: adjoin-set

(define (adjoin-set x set)
  (cond
    ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else
     (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 3 (list 2 4))   ; (2 3 4)
(adjoin-set 5 (list 1 2 3)) ; (1 2 3 5)
(adjoin-set 1 (list 2 3))   ; (1 2 3)
(adjoin-set 1 (list 1))     ; (1)
(adjoin-set 1 nil)          ; (1)
