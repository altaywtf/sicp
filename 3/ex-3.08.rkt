#lang racket

; ex-3.08: define a simple procedure f such that evaluating (+ (f 0) (f 1)) will return 0:L->R, 1:R->L

(define (make-f)
  (let ((state 1))
    (lambda (x)
      (set! state (* state x))
      state)))

(define f1 (make-f))

(+ (f1 0) (f1 1))

(define f2 (make-f))

(+ (f2 1) (f2 0))
