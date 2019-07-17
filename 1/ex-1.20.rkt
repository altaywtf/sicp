#lang sicp

; Euclid's Algorithm <3
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))


; Applicative Order - 4 Times
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 6 (remainder 40 6))
(gcd 4 (remainder 6 4))
(gcd 2 (remainder 4 2))


; Normal Order - 18 Times
(gcd 206 40)

(if (= 40 0) 206 (gcd 40 (remainder 206 40)))

(gcd 40  (remainder 206 40))

(if (= (remainder 206 40) 0) 40 ; evaluated (= 6 0) -> remainders: 1 
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(gcd (remainder 206 40)  (remainder 40 (remainder 206 40)))

(if (= (remainder 40 (remainder 206 40)) 0) (remainder 206 40) ; evaluated (= 4 0) -> remainders: (1+2) = 3
    (gcd
     (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))

(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) (remainder 40 (remainder 206 40)) ; evaluated (= 2 0) -> remainders: (3+4) = 7
    (gcd
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ; evaluated (= 0 0) -> remainders: (7+7) = 14
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) ; result (2) -> remainders: (14+4) = 18
