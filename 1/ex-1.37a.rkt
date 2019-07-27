#lang sicp

; ex-1.37a: write a k-term infinite continued fraction procedure
; which generates a recursive process

; 1 -> k
(define (cont-frac n d k)
  (define (calculate i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (calculate (+ i 1))))))
  
  (calculate 1))

; k -> 1
(define (cont-frac-t n d k)
  (if (= k 1)
      (/ (n k) (d k))
      (/ 1 (+ (/ (n k) (d k)) (cont-frac-t n d (- k 1))))))


; cont-frac for 4
(cont-frac
 (lambda (i) 1.0) (lambda (i) 1.0) 4) ; -> 0.6000000000000001

(cont-frac-t
 (lambda (i) 1.0) (lambda (i) 1.0) 4) ; -> 0.6000000000000001


; cont-frac for 1000
(cont-frac
 (lambda (i) 1.0) (lambda (i) 1.0) 1000) ; -> 0.6180339887498948

(cont-frac-t
 (lambda (i) 1.0) (lambda (i) 1.0) 1000) ; -> 0.6180339887498948


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

  (let ((result (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))
    (let ((diff (- result target)))
      (reporter k result diff)
      (if (< (abs diff) tolerance)
          k
          (find-best-k (+ k 1))))))

(find-best-k 1) ; -> 10
