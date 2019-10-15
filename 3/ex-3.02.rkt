#lang racket

; ex-3.2: wrap a fn with make-monitored to keep call-count
(define make-monitored
  (let ((call-count 0))
    (lambda (fn)
      (define (dispatch input)
        (if (eq? input 'how-many-calls?)
            call-count
            (begin (set! call-count (+ call-count 1))
                   (fn input))))
      dispatch)))


; test
(define s (make-monitored sqrt))

(s 100)
(s 'how-many-calls?) ; 1

(s 36)
(s 'how-many-calls?) ; 2

(s 16)
(s 'how-many-calls?) ; 3
