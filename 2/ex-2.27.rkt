#lang sicp

; ex-2.27: deep-reverse for nested lists

; (reverse list) from ex-2.18
(define (reverse l)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))

  (iter l nil))


; (deep-reverse l)
(define (deep-reverse l)
  (cond ((null? l) nil)
        ((not (pair? l)) l)
        (else
         (append
          (deep-reverse (cdr l))
          (list (deep-reverse (car l)))))))


(define x (list (list 1 2) (list 3 4)))
x
(deep-reverse x)

(newline)

(define z (list (list 1) (list 2) 3 (list 4)))
z
(deep-reverse z)

(newline)

(define w (list (list (list 1 2) 3) (list (list (list 4)))))
w
(deep-reverse w)
