#lang sicp

; ex-2.14, 2.15, 2.16

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


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one (add-interval (div-interval one r1) (div-interval one r2)))))


; 2.14, Lem is right
(define int-a (make-center-percent 5 0.05))
(define int-b (make-center-percent 10 0.01))

(define R1 (par1 int-a int-b))
R1           ; -> (3.06, 3.6)
(center R1)  ; -> 3.341
(percent R1) ; -> 0.08

(define R2 (par2 int-a int-b))
R2           ; -> (3.2, 3.45)
(center R2)  ; -> 3.332
(percent R2) ; -> 0.03

; testing A/A and A/B
(div-interval int-a int-a) ; not (1,1) -> (0.9, 1.1)
(div-interval (make-interval 2 8) (make-interval 2 8)) ; -> (0.25, 4)
; there's no definition of identity for intervals in our pkg

; 2.15, operations like R1/R1+R2 will introduce more uncertainity

