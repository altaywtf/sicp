#lang sicp

; ch-2.3.3: example -> representing sets

; -> sets as ordered lists

(define (element-of-set? set x)
  (cond
    ((null? set) false)
    ((= x (car set)) true)
    ((< x (car set)) false)
    (else (element-of-set? (cdr set) x))))

(define (intersection-set set1 set2)
  (if
   (or (null? set1) (null? set2))
   '()
   (let ((x1 (car set1)) (x2 (car set2)))
     (cond
       ((= x1 x2)
        (cons x1 (intersection-set (cdr set1) (cdr set2))))
       ((< x1 x2)
        (intersection-set (cdr set1) set2))
       (else
        (intersection-set set1 (cdr set2)))))))       
