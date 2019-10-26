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


; has-cycle-naive?
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
(define t1 '(a b c))
(has-cycle-naive? t1)
(has-cycle-naive? (make-cycle t1))

(define t2 '(a b c))
(has-cycle? t2)
(has-cycle? (make-cycle t2))

