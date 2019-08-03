#lang sicp

; ex-2.23: implement for-each

(define (for-each proc items)
  (if (null? items)
      (newline)
      (let ()
        (proc (car items))
        (for-each proc (cdr items)))))

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))
  