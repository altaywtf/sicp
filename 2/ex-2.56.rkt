#lang sicp

; ex-2.56: implement derivative of exponentiations

(define (=number? exp num)
  (and
   (number? exp)
   (= exp num)))

(define (make-exponentiation base exponent)
  (cond
    ((=number? exponent 0) 1)
    ((=number? exponent 1) base)
    ((and (number? base) (number? exponent)) (expt base exponent))
    (else (list '** base exponent))))

(define (exponentiation? exp)
  (and
   (pair? exp)
   (eq? (car exp) '**)))

(define (base expt-exp)
  (cadr expt-exp))

(define (exponent expt-exp)
  (caddr expt-exp))

; --> updated deriv in ch-2.3.2.rkt file to use them
