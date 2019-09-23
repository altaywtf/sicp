#lang racket

(require "./ch-2.4.3-rectangular-complex-number-operations.rkt")
(require "./ch-2.4.3-polar-complex-number-operations.rkt")
(require "./ch-2.4.3-apply-generic.rkt")
(require "./ch-2.4.3-lookup-table.rkt")

(install-rectangular-package)
(install-polar-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; test-rectangular
(define my-rectangular-complex-number
  ((get 'make-from-real-imag 'rectangular) 1 2))

(real-part my-rectangular-complex-number)
(imag-part my-rectangular-complex-number)
(magnitude my-rectangular-complex-number)
(angle my-rectangular-complex-number)

; test-polar
(define my-polar-complex-number
  ((get 'make-from-mag-ang 'polar) 3 4))

(magnitude my-polar-complex-number)
(angle my-polar-complex-number)
(real-part my-polar-complex-number)
(imag-part my-polar-complex-number)
