#lang sicp

; ex-2.57: modify make-sum and make-product to handle arbitrary numbers of terms

; helpers
(define (=number? exp num)
  (and
   (number? exp)
   (= exp num)))

; sum
(define (make-sum . arguments)  
  (define (make-sum-two a1 a2)
    (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2))))

  (define (make-sum-list l)
    (if (null? l)
        0
        (make-sum-two (car l) (make-sum-list (cdr l)))))

  (make-sum-list arguments))

(define (addend sum-exp)
  (cadr sum-exp))

(define (augend sum-exp)
  (caddr sum-exp))

(make-sum 2 3 4 5 'x)
