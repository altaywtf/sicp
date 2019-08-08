#lang sicp

; ex-2.38: fold-right / fold-left

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)


; fold-left
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter
         (op result (car rest))
         (cdr rest))))

  (iter initial sequence))


; samples
(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3))  ; 1/6

(fold-right list nil (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))  ; (((() 1) 2) 3)

; which ops would give eq results?
(=
 (fold-right + 0 (list 1 2 3))
 (fold-left + 0 (list 1 2 3)))

(=
 (fold-right * 1 (list 1 2 3))
 (fold-left * 1 (list 1 2 3)))
