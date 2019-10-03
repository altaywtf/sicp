#lang racket

; ex-2.82: update apply-generic-with-coercion to support more than 2 arguments

(require "./ch-2.4.3-lookup-table.rkt")
(require "./ch-2.5.1-scheme-number-operations.rkt")
(require "./ch-2.5.1-rational-number-operations.rkt")
(require "./ch-2.5.1-complex-number-operations.rkt")
(require "./ch-2.5.2-coercion-lookup-table.rkt")
(require "./ch-2.5.2-apply-generic-with-coercion.rkt")

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)


; integer -> rational -> complex coercions
(define (integer->rational n)
  ((get 'make 'rational) n 1))

(define (rational->complex r)
  ((get 'make-from-real-imag 'complex) (/ (cadr r) (cddr r)) 0))

(define (integer->complex n)
  (rational->complex (integer->rational n)))

(put-coercion 'scheme-number 'rational integer->rational)
(put-coercion 'rational 'complex rational->complex)
(put-coercion 'scheme-number 'complex integer->complex)


; test
(define (add x y) (apply-generic 'add x y))

(define int 5)
(define rat ((get 'make 'rational) 1 2))
(define cpx ((get 'make-from-real-imag 'complex) 3 4))

(add rat int)
; (add rat cpx)
