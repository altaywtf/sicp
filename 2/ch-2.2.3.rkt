#lang sicp

; ch-2.2.3: sequences as conventional interfaces

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (filter predictate sequence)
  (cond ((null? sequence) nil)
        ((predictate (car sequence))
         (cons (car sequence) (filter predictate (cdr sequence))))
        (else
         (filter predictate (cdr sequence)))))


(define (map fn sequence)
  (if (null? sequence)
      nil
      (cons (fn (car sequence)) (map fn (cdr sequence)))))


(define (enumarate-interval low high)
  (if (> low high)
      nil
      (cons low (enumarate-interval (+ low 1) high))))


(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

