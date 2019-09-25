#lang racket

(require "./ch-2.5.1-scheme-number-operations.rkt")
(require "./ch-2.4.3-lookup-table.rkt")
(require "./ch-2.4.3-apply-generic.rkt")

; ex-2.78: make use of internal primitive predictates for ordinary numbers (scheme-number)
(install-scheme-number-package)

(define (make-scheme-number x) ((get 'make 'scheme-number) x ))

(define (add x y) (apply-generic 'add x y))

; nummars
(define one (make-scheme-number 1))
(define two (make-scheme-number 2))

(add two
     (add one
          (add one two))) ; -> 6
