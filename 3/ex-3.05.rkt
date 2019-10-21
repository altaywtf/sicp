#lang racket

(require "./ch-3.1.2-monte-carlo.rkt")

; ex-3.5: le monte-carlo integration
; estimate definite integrals by using monte-carlo simulation
(define (square x) (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (in-circle?)
  (>= 1 (+ (square (random-in-range -1.0 1.0))
           (square (random-in-range -1.0 1.0)))))

(define (estimate-integral P x1 x2 y1 y2 trials)  
  (* (* (- x2 x1)
        (- y2 y1))
     (monte-carlo trials P)))

(define (estimate-pi)
  (estimate-integral in-circle? -1.0 1.0 -1.0 1.0 10000))

(estimate-pi)
