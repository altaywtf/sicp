#lang racket

(require "./ch-2.4.2-tagged-data.rkt")
(require "./ch-2.4.3-lookup-table.rkt")

(provide install-rational-package)
(define (install-rational-package)
  ;; internal procedures
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))

  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
              (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (numer y) (denom x))))

  ;; provided interface
  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))

  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))

  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))

  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))

  ; ex-2.79
  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (eq? (numer x) (numer y)) (eq? (denom x) (denom y)))))

  ; ex-2.80
  (put '=zero? '(rational)
       (lambda (x) (eq? (numer x) 0)))

  'installed-rational-package)
