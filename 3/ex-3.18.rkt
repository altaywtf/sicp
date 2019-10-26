#lang sicp

; ex-3.18: has-cycle?

; make-cycle from ex-3.13
(define (make-cycle x)
  (define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))

  (set-cdr! (last-pair x) x)
  x)


; has-cycle-naive? -> only checks if the (last-pair x) eq? to x
(define (has-cycle-naive? x)
  (define (loop tail)
    (cond ((null? tail) false)
          ((eq? x tail) true)
          (else (loop (cdr tail)))))

  (loop (cdr x)))


; has-cycle?
(define (has-cycle? x)
  (let ((traversed '()))
    (define (loop y)
      (cond ((null? y) false)
            ((memq y traversed) true)
            (else
             (set! traversed (cons y traversed))
             (loop (cdr y)))))
      (loop x)))

; test
(has-cycle-naive? '(a b c)) ; -> f
(has-cycle? '(a b c))       ; -> f

(has-cycle? (make-cycle '(a b c))) ; -> t
(has-cycle-naive? (make-cycle '(a b c))) ; -> t

(define y '(1 2 3 4 5 6 7 8))
(set-cdr! (cdddr (cddddr y)) (cdddr y))

(has-cycle? y) ;-> t
; (has-cycle-naive? y) ;-> boom! -> see ex-3.19


