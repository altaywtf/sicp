#lang sicp

; ex-2.2: line-segment representation in a plane, start-end points.

(define (avg v1 v2) (/ (+ v1 v2) 2))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (midpoint-segment segment)
  (let ((start (start-segment segment)) (end (end-segment segment)))
    (let ((x1 (x-point start))
          (x2 (x-point end))
          (y1 (y-point start))
          (y2 (y-point end)))
      (make-point (avg x1 x2) (avg y1 y2)))))

(define segment (make-segment (make-point 4.0 1.0) (make-point 0 2.0)))

(midpoint-segment segment) ; 2, 1.5
