#lang sicp

; ex-2.41: find ordered triplets (i, j, k) in interval of positive integers 1 -> n where (i + j + k = s)

; helpers
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap op sequence)
  (accumulate append nil (map op sequence)))

(define (filter predictate sequence)
  (cond ((null? sequence) nil)
        ((predictate (car sequence))
         (cons (car sequence) (filter predictate (cdr sequence))))
        (else
         (filter predictate (cdr sequence)))))

(define (enumarate-interval low high)
  (if (> low high)
      nil
      (cons low (enumarate-interval (+ low 1) high))))


; unique-pairs
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumarate-interval 1 (- i 1))))
   (enumarate-interval 1 n)))

; unque-triplets (TEMP)
(define (unique-triplets n)
  (accumulate
   (lambda (x y)
     (let ((pairs (unique-pairs (- x 1))))
       (if (null? pairs)
           y
           (append
            (map (lambda (pair) (append (list x) pair)) pairs) y))))
   nil
   (enumarate-interval 1 n)))


(define (is-sum-equal? target)
  (lambda (sequence)
    (= target (accumulate + 0 sequence))))


(define (find-ordered-triplets-sum-to n s)
  (filter (is-sum-equal? s) (unique-triplets n)))


; test
(find-ordered-triplets-sum-to 5 7)
(find-ordered-triplets-sum-to 6 10)
