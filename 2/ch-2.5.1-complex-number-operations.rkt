#lang racket

(require "./ch-2.4.2-tagged-data.rkt")
(require "./ch-2.4.3-lookup-table.rkt")
(require "./ch-2.4.3-polar-complex-number-operations.rkt")
(require "./ch-2.4.3-rectangular-complex-number-operations.rkt")
(require "./ch-2.4.3-apply-generic.rkt")

(provide install-complex-package)
(define (install-complex-package)
  ;;; imported procedures
  (install-polar-package)
  (install-rectangular-package)

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))

  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))

  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))

  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))

  ; ex-2.77
  (put 'real-part '(complex)
       (lambda (z) (apply-generic 'real-part z)))
  
  (put 'imag-part '(complex)
       (lambda (z) (apply-generic 'imag-part z)))

  (put 'magnitude '(complex)
       (lambda (z) (apply-generic 'magnitude z)))

  (put 'angle '(complex)
       (lambda (z) (apply-generic 'angle z)))

  ; ex-2.79
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (apply-generic 'equ? z1 z2)))

  ; ex-2.80
  (put '=zero? '(complex)
       (lambda (z)
         (= (apply-generic 'real-part z) (apply-generic 'imag-part z) 0)))

  'installed-complex-package)
