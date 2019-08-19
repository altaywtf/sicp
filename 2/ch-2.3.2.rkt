#lang sicp

; ch-2.3.2: symbolic differentiation

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
    (else (list '+ a1 a2))))

(define (addend sum-exp)
  (cadr sum-exp))

(define (augend sum-exp)
  (caddr sum-exp))

(define (sum? x)
  (and
   (pair? x)
   (eq? (car x) '+)))

; product
(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list '* m1 m2))))

(define (multiplier pro-exp)
  (cadr pro-exp))

(define (multiplicand pro-exp)
  (caddr pro-exp))

(define (product? x)
  (and
   (pair? x)
   (eq? (car x) '*)))

; exponentiation
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

        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        
        (else
         (error "unkonwn expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x) ; 1
(deriv '(** x 2) 'x) ; (* 2 x)
(deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))
