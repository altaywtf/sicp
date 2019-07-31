#lang sicp

; ex-2.10: refactor divide procedure to throw error if any of bound y is zero

(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (let ((a (car interval)) (b (cdr interval)))
    (if (< a b) a b)))

(define (upper-bound interval)
  (let ((a (car interval)) (b (cdr interval)))
    (if (< a b) b a)))

(define (get-width interval)
  (abs (/ (- (lower-bound interval) (upper-bound interval)) 2.0)))

(define (add-interval x y)
  (make-interval
   (+ (lower-bound x) (lower-bound y))
   (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval
   (- (lower-bound x) (upper-bound y))
   (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval
     (min p1 p2 p3 p4)
     (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (= 0 (upper-bound y)) (= 0 (lower-bound y)))
      (error "Division by 0" y)
      (mul-interval
       x (make-interval
          (/ 1.0 (upper-bound y))
          (/ 1.0 (lower-bound y))))))

; test
(define i1 (make-interval 10 15))
(define i2 (make-interval 0 5))
(div-interval i2 i1)
(div-interval i1 i2)


