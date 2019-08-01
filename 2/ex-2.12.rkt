#lang sicp

; ex-2.12: implement (make-center-percent center-of-the-integral error-tolerance)

(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (let ((a (car interval)) (b (cdr interval)))
    (if (< a b) a b)))

(define (upper-bound interval)
  (let ((a (car interval)) (b (cdr interval)))
    (if (< a b) b a)))

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

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2.0))

(define (center interval)
  (/ (+ (lower-bound interval) (upper-bound interval)) 2.0))

(define (make-center-width center width)
  (make-interval (- center width) (+ center width)))

(define (make-center-percent center percent)
  (make-center-width center (* center percent)))

(define (percent integral)
  (/ (width integral) (center integral)))

; test
(define my-int (make-center-percent 1 0.5))
my-int ; (0.5, 1.5)
(percent my-int) ; 0.5


(define my-int2 (make-center-percent 0 0.5))
my-int2 ; (0, 0)
(percent my-int2) ; error :D

