#lang racket

(require "./ch-2.4.2-tagged-data.rkt")
(require "./ch-2.4.3-lookup-table.rkt")

(provide install-scheme-number-package)
(define (install-scheme-number-package)
  ;; provided interface
  (define (tag x) (attach-tag 'scheme-number x))

  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))

  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))

  ; ex-2.79
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (eq? x y)))

  ; ex-2.80
  (put '=zero? '(scheme-number)
       (lambda (x) (eq? x 0)))

  'installed-scheme-number-package)
