#lang racket

; ex-3.3: modify make-account with password

(define (make-account balance initial-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (get-method m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown method" m))))

  (define (valid-password? p)
    (eq? p initial-password))  

  (define (dispatch password method)
    (if (valid-password? password)
        (get-method method)
        (lambda _ "Incorrect password")))
  
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 50)

((acc 'invalid-password 'withdraw) 50)
