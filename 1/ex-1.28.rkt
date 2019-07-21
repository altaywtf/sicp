#lang sicp

; ex-1.28: implement Miller-Rabin test
(define (square x) (* x x))
(define (is-even? x) (= 0 (remainder x 2)))
(define (square-mod x n) (remainder (square x) n))

(define (square-signal-root x n)
  (if (and
       (not (= 1 x))
       (not (= x (- n 1)))
       (= 1 (square-mod x n)))
      0
      (square-mod x n)))

(define (expmod base exponent modulo)
  (cond ((= 0 exponent) 1)
        ((is-even? exponent)
         (square-signal-root (expmod base (/ exponent 2) modulo) modulo))
        (else
         (remainder (* base (expmod base (- exponent 1) modulo)) modulo))))

(define (miller-rabin-test n)
  (define (iter a)
    (cond ((= a n) #t)
          ((= (expmod a (- n 1) n) 1)
           (iter (+ a 1)))
          (else #f)))
  (iter 1))


; reporter
(define (report-miller-rabin-test n is-prime-expected)
  (display n)
  (if (eq? (miller-rabin-test n) is-prime-expected)
      (display " -> passed ✓✓✓")
      (display " -> fooled :-o"))
  (newline))


; test with random numbers
(report-miller-rabin-test 4 false)
(report-miller-rabin-test 7 true)
(report-miller-rabin-test 19 true)
(report-miller-rabin-test 25 false)
(report-miller-rabin-test 91 false)
(report-miller-rabin-test 1730 false)
(display "--- \n")

; test with Carmichael numbers
(report-miller-rabin-test 561 false)
(report-miller-rabin-test 1105 false)
(report-miller-rabin-test 1729 false)
(report-miller-rabin-test 2465 false)
(report-miller-rabin-test 2821 false)
(report-miller-rabin-test 6601 false)
