#lang racket

; make-account
(require "./ex-3.03.rkt")

; ex-3.7: make-joint bank account
(define (make-joint master-acc master-pass sub-pass)
  (define (verify-password p)
    (eq? p sub-pass)) 
  
  (define (dispatch p m)
    (if (verify-password p)
        (master-acc master-pass m)
        (lambda _ "Incorrect password")))

  dispatch)
  

; test
(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 10) ; 90
((paul-acc 'rosebud 'withdraw) 50) ; 40
