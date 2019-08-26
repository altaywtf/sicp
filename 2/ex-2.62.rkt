#lang sicp

; ex-2.62: O(n) union-set

(define (union-set set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
     (let ((x1 (car set1)) (x2 (car set2)))
       (cond
         ((= x1 x2)
          (cons x1 (union-set (cdr set1) (cdr set2))))
         ((< x1 x2)
          (cons x1 (union-set (cdr set1) set2)))
         (else
          (cons x2 (union-set set1 (cdr set2)))))))))

(union-set (list 1 4) (list 2 3 4 5)) ; (1 2 3 4 5)
(union-set (list 3 4) (list 2 3 4 5)) ; (2 3 4 5)
(union-set nil (list 1 2))            ; (1 2)
(union-set (list 4 5) nil)            ; (4 5)
