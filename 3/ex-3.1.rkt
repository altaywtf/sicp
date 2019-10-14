#lang racket

; ex-3.1: make-accumulator

(define (make-accumulator value)
  (lambda (to-sum)
    (begin (set! value (+ value to-sum))
           value)))

(define A (make-accumulator 5))

(A 10) ; 15

(A 10) ; 25
