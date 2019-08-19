#lang sicp

; ch-2.3.2: symbolic differentiation

(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and
   (variable? v1)
   (variable? v2)
   (eq? v1 v2)))

; sum
(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (addend sum-exp)
  (cadr sum-exp))

(define (augend sum-exp)
  (caddr sum-exp))

(define (sum? x)
  (and
   (pair? x)
   (eq? (car x) '+)))

; product
(define (make-product a1 a2)
  (list '* a1 a2))

(define (multiplier pro-exp)
  (cadr pro-exp))

(define (multiplicand pro-exp)
  (caddr pro-exp))

(define (product? x)
  (and
   (pair? x)
   (eq? (car x) '*)))


; derivative
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-product (deriv (multiplicand exp) var)
                       (deriv (multiplier exp) var)))
        (else
         (error "unkonwn expression type -- DERIV" exp))))

