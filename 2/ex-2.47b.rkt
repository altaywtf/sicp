#lang sicp

; ex-2.47: selectors for frame constructor

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (car (cdr f)))

(define (edge2-frame f)
  (cdr (cdr f)))


; test
(define frame (make-frame 0 1 2))
(origin-frame frame)
(edge1-frame frame)
(edge2-frame frame)
