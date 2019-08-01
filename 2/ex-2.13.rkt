#lang sicp

; ex-2.13: show the correlation between the tolerance of two intervals and the tolerance of multiplication of two intervals

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


; reporter
(define (report-tolerances int-a int-b)
  (newline)
  (display "tolerance int-a: ")
  (display (percent int-a))
  (newline)
  (display "tolerance int-b: ")
  (display (percent int-b))
  (newline)
  (display "tolerance mul: ")
  (percent (mul-interval int-a int-b)))


(define int-a (make-center-percent 5 0.05))
(define int-b (make-center-percent 10 0.001))

(report-tolerances int-a int-b) ; 0.05, 0.0009, 0.0509 -> tol(a) + tol(b)
