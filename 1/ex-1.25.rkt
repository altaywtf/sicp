#lang sicp

; compare expmod with fast-expt
 (define (square x)
   (* x x))

(define (is-even? x)
  (= 0 (remainder x 2)))


; expmod we use in fermat test
(define (expmod base exponent modulo)
  (cond ((= 0 exponent) 1)
        ((is-even? exponent)
         (remainder (square (expmod base (/ exponent 2) modulo)) modulo))
        (else
         (remainder (* base (expmod base (- exponent 1) modulo)) modulo))))


; expmod that uses fast-expt
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((is-even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))

(define (alt-expmod base exponent modulo)
  (remainder (fast-expt base exponent) modulo))


; singular comparison of expmod - alt-expmod
(define (timed-calculation procedure b e m start-time)
  (display (procedure b e m))
  (display ": elapsed time: ")
  (display (- (runtime) start-time))
  (display "\n---\n"))

; (timed-calculation expmod 7 109 109 (runtime))
; - square of 7
; - square of 16
; - square of 38
; - square of 80
; - square of 1
; - square of 1
; 7: elapsed time: 1220

; (timed-calculation alt-expmod 7 109 109 (runtime))
; - square of 7
; - square of 343
; - square of 117649
; - square of 96889010407
; - square of 65712362363534280139543
; - square of 4318114567396436564035293097707728087552248849
; 7: elapsed time: 1238

; plug expmods to prime test
(define (fermat-test n modder)
  (define (try-it a)
    (= (modder a n n) a))
  (define random-number
    (+ 1 (random (- n 1))))
  (try-it random-number))

(define (fast-prime? n times modder)
  (cond ((= times 0) true)
        ((fermat-test n modder) (fast-prime? n (- times 1) modder))
        (else false)))

(define (timed-prime-test n modder)
  (start-prime-test n modder (runtime)))

(define (start-prime-test n modder start-time)
  (if (fast-prime? n 3 modder)
      (report-prime n (- (runtime) start-time))
      #f))

(define (report-prime n elapsed-time)
  (display n)
  (display ": elapsed time: ")
  (display elapsed-time)
  (newline))

(define (search-for-prime n modder)
  (search-for-primes n 3 modder))

(define (search-for-primes n counter modder)
  (if (> counter 0)
      (if (timed-prime-test n modder)
          (search-for-primes (+ n 1) (- counter 1) modder)
          (search-for-primes (+ n 1) counter modder))
      (display "--- \n")))

; comparison
(search-for-prime 100000 expmod)
; -> 100003: elapsed time: 8
; -> 100019: elapsed time: 9
; -> 100043: elapsed time: 9
; expmod is recursive, where each recursive call returns the value (remainder (square x) m), with x as the value of the next recursive call.
; the resulting numbers are therefore always less than m2.

(search-for-prime 100000 alt-expmod)
; -> 100003: elapsed time: 114232
; -> 100019: elapsed time: 110464
; -> 100043: elapsed time: 71786
; fast-expt is also recursive, but each recursive call returns a value that is the square of the next recursive call and these numbers grow.

; https://github.com/kean/SICP/blob/master/Chapter%201/sicp-exercise-1-25.scm
