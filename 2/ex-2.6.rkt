#lang sicp

; ex-2.6: implement one and two by using substition to evaluate (add-1 zero)

(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))


(add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x)) f) x)))
; (lambda (f) (lambda (x) (f x)))

(define one
  (lambda (f)
    (lambda (x) (f x))))

(add-1 one)
; (lambda (f) (lambda (x) (f f (x))))

(define two
  (lambda (f)
    (lambda (x) (f (f x)))))

(add-1 two)
; (lambda (f) (lambda (x) (f f f (x))))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))
