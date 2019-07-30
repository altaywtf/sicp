#lang sicp

; ex-2.3: rectangle representation with perimeter and area calculations

; from ex-2.2
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

; public -> (make-rect top-left bottom-right)
(define (make-rect top-left bottom-right)
  (cons top-left bottom-right))

; internal accessors
(define (top-left rect)
  (car rect))

(define (bottom-right rect)
  (cdr rect))

(define (bottom-left rect)
  (make-point (x-point (top-left rect)) (y-point (bottom-right rect))))

(define (top-right rect)
  (make-point (x-point (bottom-right rect)) (y-point (top-left rect))))

; public -> (width-rect rect)
(define (width-rect rect)
  (abs (- (x-point (top-left rect)) (x-point (top-right rect)))))

; public -> (height-rect rect)
(define (height-rect rect)
  (abs (- (y-point (top-left rect)) (y-point (bottom-left rect)))))

; public -> (perimeter-rect rect)
(define (perimeter-rect rect)
  (* 2 (+ (width-rect rect) (height-rect rect))))

; public -> (area-rect rect)
(define (area-rect rect)
  (* (width-rect rect) (height-rect rect)))

(define rect (make-rect (make-point 0 0) (make-point 3 (- 5))))
(perimeter-rect rect) ; -> 16
(area-rect rect)      ; -> 15


; alternative implementation for make-rect
(define (make-rect-alt top-left width height)
  (cons top-left
        (make-point (+ (car top-left) width) (- (cdr top-left) height))))

(define alt-rect (make-rect-alt (make-point 0 0) 3 5))
(perimeter-rect alt-rect) ; -> 16
(area-rect alt-rect)      ; -> 15
