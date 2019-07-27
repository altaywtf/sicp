#lang sicp

; ex-1.37b: write a k-term infinite continued fraction procedure
; which generates an iterative process

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))

  (iter k 0))


; cont-frac-iter for 4
(cont-frac-iter
 (lambda (i) 1.0) (lambda (i) 1.0) 4)   ; -> 0.6000000000000001

; cont-frac-iter for 1000
(cont-frac-iter
 (lambda (i) 1.0) (lambda (i) 1.0) 100) ; -> 0.6180339887498948


; find-best-k
(define (reporter k result diff)
  (display "k: ")
  (display k)
  (newline)
  (display "result: ")
  (display result)
  (newline)
  (display "diff: ")
  (display diff)
  (newline)
  (display "---- \n"))

(define (find-best-k k)
  (define target (/ 1 1.61803398875))
  (define tolerance 0.0001)

  (let ((result (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) k)))
    (let ((diff (- result target)))
      (reporter k result diff)
      (if (< (abs diff) tolerance)
          k
          (find-best-k (+ k 1))))))

(find-best-k 1) ; -> 10
