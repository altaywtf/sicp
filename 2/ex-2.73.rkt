#lang racket
(require "./ex-2.73-lookup-extension.rkt")
(require "./ch-2.3.2.rkt")

; ex-2.73: symbolic differentiation with generic deriv

; a -> number? and same-variable? are predicates. nothing to dispatch.

; b -> sum-package
(define (install-sum-package)
  (define (addend operands) (car operands))
  (define (augend operands) (cadr operands))
  
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  
  (put 'deriv '+ deriv-sum))


; b -> product-package
(define (install-product-package)
  (define (multiplier operands) (car operands))
  (define (multiplicand operands) (cadr operands))

  (define (deriv-product operands var)
    (make-sum
     (make-product (multiplier operands)
                   (deriv (multiplicand operands) var))
     (make-product (deriv (multiplier operands) var)
                   (multiplicand operands))))

  (put 'deriv '* deriv-product))


; c -> exp-package
(define (install-exp-package)
  (define (base operands) (car operands))
  (define (exponent operands) (cadr operands))
  
  (define (make-exponentiation base exponent)
    (cond
      ((=number? exponent 0) 1)
      ((=number? exponent 1) base)
      ((and (number? base) (number? exponent)) (expt base exponent))
      (else (list '** base exponent))))

  (define (deriv-exp operands var)
    (make-product
     (make-product (exponent operands)
                   (make-exponentiation (base operands) (make-sum (exponent operands) -1)))
     (deriv (base operands) var)))

  (put 'deriv '** deriv-exp))


;;;
(install-sum-package)
(install-product-package)
(install-exp-package)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))


; examples
(deriv '(+ x 3) 'x) ; 1
(deriv '(** x 2) 'x) ; (* 2 x)
(deriv '(+ (** x 2) (* y x)) 'x) ; (+ (* 2 x) y)
(deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))
