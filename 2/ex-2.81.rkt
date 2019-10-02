#lang racket

; ex-2.81: type coercion issues in new apply-generi

(require "./ch-2.4.3-lookup-table.rkt")
(require "./ch-2.5.1-scheme-number-operations.rkt")
(require "./ch-2.5.1-complex-number-operations.rkt")
(require "./ch-2.5.2-coercion-lookup-table.rkt")
(require "./ch-2.5.2-apply-generic-with-coercion.rkt")

(install-scheme-number-package)
(install-complex-package)


; pre-work of Louis
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)


; a -> undefined ops for coerced types
(define (exp x y) (apply-generic 'exp x y))

; works for scheme-number
(exp 2 4)

(define (make-complex-number-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define cr (make-complex-number-from-real-imag 1 2))

(exp cr cr) ; -> throws arity mismatch error

; b -> do we need to coerce args with the same type?
; nope, it works as it is.
; because it finds the procedure to apply without going to the coercion flow.

; c -> done in place
