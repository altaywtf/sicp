#lang sicp

; ex-1.27: fool the Fermat test with Carmichael numbers
(define (square x) (* x x))
(define (is-even? x) (= 0 (remainder x 2)))

(define (expmod base exponent modulo)
  (cond ((= 0 exponent) 1)
        ((is-even? exponent)
         (remainder (square (expmod base (/ exponent 2) modulo)) modulo))
        (else
         (remainder (* base (expmod base (- exponent 1) modulo)) modulo))))

(define (fermat-test n)
  (define (iter a)
    (cond  ((= a n) #t)
           ((= (expmod a n n) a) (iter (+ a 1)))
           (else #f)))
  (iter 1))


; reporter
(define (report-fermat-test n is-prime-expected)
  (display n)
  (if (eq? (fermat-test n) is-prime-expected)
      (display " -> passed ✓✓✓")
      (display " -> fooled :-o"))
  (newline))


; test with random numbers
(report-fermat-test 4 false)
(report-fermat-test 7 true)
(report-fermat-test 19 true)
(report-fermat-test 25 false)
(report-fermat-test 91 false)
(report-fermat-test 1730 false)
(display "--- \n")

; test with Carmichael numbers
(report-fermat-test 561 false)
(report-fermat-test 1105 false)
(report-fermat-test 1729 false)
(report-fermat-test 2465 false)
(report-fermat-test 2821 false)
(report-fermat-test 6601 false)
