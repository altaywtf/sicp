#lang racket
(require "./ex-2.73-lookup-extension.rkt")

; ex-2.73: symbolic differentiation with generic deriv

; helpers
(define (=number? exp num)
  (and
   (number? exp)
   (= exp num)))

(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and
   (variable? v1)
   (variable? v2)
   (eq? v1 v2)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; sum-package
(define (install-sum-package)
  (define (make-sum a1 a2)
    (cond
      ((=number? a1 0) a2)
      ((=number? a2 0) a1)
      ((and (number? a1) (number? a2)) (+ a1 a2))
      (else (list '+ a1 a2))))

  (define (addend operands) (car operands))
  (define (augend operands) (cadr operands))
  
  (define (deriv-sum operands var)
    (make-sum (deriv (car operands) var)
              (deriv (cadr operands) var)))
  
  (put 'deriv '+ deriv-sum)
  
  'sum-package-installed)

(install-sum-package)



(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))


; derivative
#|
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
           (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        
        (else
         (error "unkonwn expression type -- DERIV" exp))))
|#

(deriv '(+ x 3) 'x) ; 1
; (deriv '(** x 2) 'x) ; (* 2 x)
; (deriv '(+ (** x 2) (* y x)) 'x) ; (+ (* 2 x) y)
; (deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))

