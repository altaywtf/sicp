#lang racket

; ex-3.4: modify make-account with password to call the cops after several invalid attempts

(define (make-account balance initial-password)
  (define consecutive-incorrect-password-entry-count 0)
  
  (define (call-the-cops) "CALLING THE COPSSSS U BETTER RUN XD")
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount)) balance)

  (define (get-method m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown method" m))))

  (define (valid-password? p)
    (eq? p initial-password))

  (define (handle-valid-password)
    (set! consecutive-incorrect-password-entry-count 0))
    
  (define (handle-invalid-password)
    (if (>= consecutive-incorrect-password-entry-count 2)
        (call-the-cops)
        (begin (set! consecutive-incorrect-password-entry-count (+ 1 consecutive-incorrect-password-entry-count))
               "Invalid password")))
   
  (define (dispatch password method)
    (if (valid-password? password)
        (begin (handle-valid-password) (get-method method))
        (lambda _ (handle-invalid-password))))
  
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 50)

((acc 'invalid-password 'withdraw) 5)
((acc 'invalid-password 'withdraw) 5)
((acc 'invalid-password 'withdraw) 5)
