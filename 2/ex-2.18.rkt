#lang sicp

; ex-2.18: implement (reverse list) procedure

(define my-list (list 1 4 9 16 25))

(define (reverse l)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))

  (iter l nil))


(reverse my-list)
