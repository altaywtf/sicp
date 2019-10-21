#lang racket

; ex-3.06: random number generator with reset
(define (rand)
  (define rand-init 0)
  (define (rand-update x) (+ x 1))

  (let ((seed rand-init))
    (define (generate)
      (set! seed (rand-update seed)) seed)

    (define (reset new-value)
      (set! seed new-value) seed)

    (define (dispatch m)
      (cond
        ((eq? m 'generate) (generate))
        ((eq? m 'reset) reset)
        (else (error "Unkonwn method"))))

  dispatch))


; test
(define r (rand))

(r 'generate)
(r 'generate)

((r 'reset) 5)
(r 'generate)
