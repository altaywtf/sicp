#lang sicp

; ex-2.9

; make-interval, lower-bound, upper-bound from ex-2.7
(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (let ((a (car interval)) (b (cdr interval)))
    (if (< a b) a b)))

(define (upper-bound interval)
  (let ((a (car interval)) (b (cdr interval)))
    (if (< a b) b a)))

; pre-defined procedures from ch-2.1.4
(define (add-interval x y)
  (make-interval
   (+ (lower-bound x) (lower-bound y))
   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval
     (min p1 p2 p3 p4)
     (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x (make-interval (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y)))))

; sub-interval from ex-2.8
(define (sub-interval x y)
  (make-interval
   (- (lower-bound x) (upper-bound y))
   (- (upper-bound x) (lower-bound y))))

; width selector for interval from ex-2.9
(define (get-width interval)
  (abs (/ (- (lower-bound interval) (upper-bound interval)) 2.0)))

; test
(define i1 (make-interval 0 6))
(define i2 (make-interval 2 10))

(get-width i1) ; 3
(get-width i2) ; 4

(add-interval i1 i2) ; (2, 16)
(get-width (add-interval i1 i2)) ; 7 -> width = width-i1 + width-i2

(sub-interval i1 i2) ; (-10, 4)
(get-width (sub-interval i1 i2)) ; 7 -> width = width-i1 + width-i2

(mul-interval i1 i2) ; (0, 60)
(get-width (mul-interval i1 i2)) ; 30

(div-interval i1 i2) ; (0, 3)
(get-width (div-interval i1 i2)) ; 1.5

