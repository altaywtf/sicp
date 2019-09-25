#lang racket

(require "./ch-2.5.1-complex-number-operations.rkt")
(require "./ch-2.4.3-rectangular-complex-number-operations.rkt")
(require "./ch-2.4.3-lookup-table.rkt")
(require "./ch-2.4.3-apply-generic.rkt")

; ex-2.77: missing selectors for complex numbers
(install-complex-package)

#|
- in order to make it work, we need to define selectors in complex-number package which are also using apply-generic.
- so, those selectors for complex numbers are calling apply-generic 2 times
|#

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; complex number in figure 2.24
(define z (make-complex-from-real-imag 3 4))

z ; -> '(complex rectangular 3 . 4)

(magnitude z) ; -> 5
(angle z)     ; -> 0.92...
