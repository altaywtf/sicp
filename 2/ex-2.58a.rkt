#lang sicp

; ex-2.58a: update diff to work with ordinary mathematical notations such as (x + (3 * (x + (y + 2))))

; helpers
(define (=number? exp num)
  (and
   (number? exp)
   (= exp num)))

; variable
(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and
   (variable? v1)
   (variable? v2)
   (eq? v1 v2)))

; sum
(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list a1 '+ a2))))

(define (addend sum-exp)
  (car sum-exp))

(define (augend sum-exp)
  (caddr sum-exp))

(define (sum? x)
  (and
   (pair? x)
   (eq? (cadr x) '+)))

; product
(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list m1 '* m2))))

(define (multiplier pro-exp)
  (car pro-exp))

(define (multiplicand pro-exp)
  (caddr pro-exp))

(define (product? x)
  (and
   (pair? x)
   (eq? (cadr x) '*)))

; derivative
(define (deriv exp var)
  (cond ((number? exp) 0)

        ((variable? exp)
         (if (same-variable? exp var) 1 0))

        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        
        (else
         (error "unkonwn expression type -- DERIV" exp))))

(deriv '(x + 3) 'x)
(deriv '(x * y) 'x)
(deriv '(x + (3 * (x + (y + 2)))) 'x)
