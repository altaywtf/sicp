#lang sicp

; has-cycle? with o(1) space

; has-cycle?
; - floyd's tortoise and hare: https://en.wikipedia.org/wiki/Cycle_detection
; - https://miro.medium.com/max/1560/1*clbAFjEFicLYjsq4pVVP4g.gif

; linked-list -> (pair)
; circular linked-List -> (make-cycle pair)

(define (has-cycle? x)
  (define (next p)
    (if (pair? p)
        (cdr p)
        '()))

  (define (next-next p)
    (next (next p)))
  
  (define (loop tortoise hare)
    (cond ((null? tortoise) false)
          ((null? hare) false)
          ((eq? tortoise hare) true)
          ((eq? tortoise (next hare)) true)
          (else
           (loop (next tortoise)
                 (next-next hare)))))

  (loop (next x) (next-next x)))


; test
(define x '(1 2 3 4)) 
(define y '(1 2 3 4))
(set-cdr! (cdr (cddr y)) (cddr y)) 

(has-cycle? x) ; -> f
(has-cycle? y) ; -> t


(define u '(1 2 3 4))
(define v '(1 2 3 4))
(set-cdr! (cdddr v) v) ; identical to make-cycle

(has-cycle? u) ; -> f
(has-cycle? v) ; -> t

