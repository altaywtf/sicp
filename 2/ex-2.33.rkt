#lang sicp

; ex-2.33: implement map, append and lenght with accumulate

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; map
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map (lambda (x) (+ x 1)) (list 1 2 3 4))


; append
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))


; length
(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(length (list 0 1 2 3 4))
