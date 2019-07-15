#lang sicp

; https://codology.net/post/sicp-solution-exercise-1-13/

(define (alpha)
  (/ (+ 1 (sqrt 5)) 2))

(define (beta)
  (/ (- 1 (sqrt 5)) 2))


(define (fib n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (else (+ (fib (- n 1))
            (fib (- n 2))))))

(define (teta n)
  (/
   (-
    (expt (alpha) n)
    (expt (beta) n))
   (sqrt 5)))

(fib 20)
(teta 20)