#lang racket

; ex-2.80: define =zero? for number packages

(require "./ch-2.4.3-apply-generic.rkt")
(require "./ch-2.4.3-lookup-table.rkt")
(require "./ch-2.5.1-scheme-number-operations.rkt")
(require "./ch-2.5.1-rational-number-operations.rkt")
(require "./ch-2.5.1-complex-number-operations.rkt")

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

; generic equ?
(define (=zero? x) (apply-generic '=zero? x))


; scheme-number
(define (make-scheme-number x)
  ((get 'make 'scheme-number) x))

(define s (make-scheme-number 0))
(=zero? s)


; rational-number
(define (make-rational-number numer denom)
  ((get 'make 'rational) numer denom))

(define r (make-rational-number 0 2))
(=zero? r)


; complex-number
(define (make-complex-number-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define cr (make-complex-number-from-real-imag 0 0))

(=zero? cr)

(define (make-complex-number-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define cp (make-complex-number-from-mag-ang 0 0))

(=zero? cp)
