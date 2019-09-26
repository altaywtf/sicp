#lang racket

(require "./ch-2.4.2-tagged-data.rkt")
(require "./ch-2.4.3-lookup-table.rkt")

(provide install-rectangular-package)
(define (install-rectangular-package)
  ;; internal procedures
  (define (square x) (* x x))
  
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))

  (define (angle z)
    (atan (imag-part z) (real-part z)))
  
  (define (make-from-real-imag x y)
    (cons x y))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; provided interface
  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'rectangular
       (lambda (x y) (tag (make-from-mag-ang x y))))

  ; ex-2.79
  (put 'equ? '(rectangular rectangular)
       (lambda (z1 z2)
         (and (eq? (real-part z1) (real-part z2))
              (eq? (imag-part z1) (imag-part z2))))) 

  'installed-rectangular-package)
