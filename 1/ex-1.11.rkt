#lang sicp

; Exercise 1.11
; Write recursive - one iterative process for
; f(n) => n < 3 ? n : f(n-1) + 2f(n-2) + 3f(n-3)

; Recursive
(define (fr n)
  (if
   (< n 3) n
   (+
    (fr (- n 1))
    (* 2 (fr (- n 2)))
    (* 3 (fr (- n 3))))))

(fr 3)
(fr 4)
(fr 10)

; Iterative
(define (fi-iter current target sum)
  (if
   (= current target) sum
   (fi-iter (+ current 1) target (+ sum (* (- target current)
                                           (if (< current 3) current
                                               (fi-iter (- current 3) current 0)))))))

(define (fi n) (fi-iter (- n 3) n 0))

(fi 3)
(fi 4)
(fi 10)