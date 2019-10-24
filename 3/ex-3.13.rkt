#lang sicp

; ex-3.13: make-cycle

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; cdr of the last item of the list points to the first cell of the list :/
(define z (make-cycle (list 'a 'b 'c)))

; (last-pair z) -> infinite recursion
