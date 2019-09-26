#lang racket

; ex-2.79: define equ? for number packages

(require "./ch-2.4.3-apply-generic.rkt")
(require "./ch-2.4.3-lookup-table.rkt")
(require "./ch-2.5.1-scheme-number-operations.rkt")
(require "./ch-2.5.1-rational-number-operations.rkt")
(require "./ch-2.5.1-complex-number-operations.rkt")

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

; generic equ?
(define (equ? x y) (apply-generic 'equ? x y))


; scheme-number
(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))

(define sx (make-scheme-number 1))
(define sy (make-scheme-number 2))
(define sz (make-scheme-number 1))  

(equ? sx sy) ; #f
(equ? sx sz) ; #t


; rational-number
(define (make-rational-number numer denom)
  ((get 'make 'rational) numer denom))

(define rx (make-rational-number 1 2))
(define ry (make-rational-number 1 3))
(define rz (make-rational-number 2 4))

(equ? rx ry) ; #f
(equ? rx rz) ; #t


; complex-number
(define (make-complex-number-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define crx (make-complex-number-from-real-imag 3 4))
(define cry (make-complex-number-from-real-imag 3 4))

(equ? crx cry) ; #t -_-

(define (make-complex-number-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define cpx (make-complex-number-from-mag-ang 1 2))
(define cpy (make-complex-number-from-mag-ang 1 2))

(equ? cpx cpy) ; #t -_-
