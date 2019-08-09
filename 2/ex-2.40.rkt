#lang sicp

; ex-2.40: unique-pairs -> given an integer n, generates the sequence of pairs (i, j) with (1 <= j <= i <= n)
; then use it for prime-sum-pairs

; is-prime? with smallest-divisor implementation O(√n) from ex-1.22
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? divisor n)
  (= 0 (remainder n divisor)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))))

(define (is-prime? n)
  (= n (smallest-divisor n)))


; higher-order procedures
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


; prime-sum-pairs
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (is-prime? (+ (car pair) (cadr pair))))

(define (prime-sun-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sun-pairs 6)
