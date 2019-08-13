#lang sicp

; ex-2.48: segment constructor and selectors
(define (make-segment start-vector end-vector)
  (cons start-vector end-vector))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
