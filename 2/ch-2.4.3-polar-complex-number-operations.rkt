#lang racket

(require "./ch-2.4.2-tagged-data.rkt")
(require "./ch-2.4.3-lookup-table.rkt")

(provide install-polar-package)
(define (install-polar-package)
  ;; internal procedures
  (define (square x) (* x x))
  
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))

  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  (define (make-from-mag-ang r a)
    (cons r a))

  ;; provided interface
  (define (tag x) (attach-tag 'polar x))

  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'polar
       (lambda (x y) (tag (make-from-mag-ang x y))))

  ; ex-2.79
  (put 'equ? '(polar polar)
       (lambda (z1 z2)
         (and (eq? (magnitude z1) (magnitude z2))
              (eq? (angle z1) (angle z2)))))

  'installed-polar-package)
