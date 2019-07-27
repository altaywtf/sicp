#lang sicp

; ex-1.39: use cont-frac to approximate tan(x) based on J.H. Lambert's cont. fraction representation

; from ex-1.37b
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))

  (iter k 0))


; tan-cf
(define (tan-cf x k)
  (cont-frac
   (lambda (i)
     (if (= i 1)
         x
         (* x x -1)))
   (lambda (i)
     (- (* 2.0 i) 1))
   k))


; tan(1)       -> 1.55740772465
(tan-cf 1 2) ; -> 1.499
(tan-cf 1 5) ; -> 1.5574074074074076

; tan(225)          -> -2.53211499233
(tan-cf 225 10)   ; -> -0.2494769427386107
(tan-cf 225 100)  ; -> -1.1399707147331366
(tan-cf 225 1000) ; -> -2.532114992334343
