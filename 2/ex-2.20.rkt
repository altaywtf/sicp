#lang sicp

; ex-2.20: (same-parity first ...rest) => filter rest based on parity of first

(define (filter predictate? my-list)
  (define (iter items)
    (if (null? items)
        nil
        (let ((current-item (car items)) (rest (cdr items)))
          (if (predictate? current-item)
              (cons current-item (iter rest))
              (iter rest)))))

  (iter my-list))


(define (same-parity first . rest)
    (define (predictate i)
      (define (is-even? i) (= 0 (remainder i 2)))
      (eq? (is-even? first) (is-even? i)))

  (cons first (filter predictate rest)))

(same-parity 1 2 3 4 5 6 7) ; -> 1 3 5 7
(same-parity 2 3 4 5 6 7)   ; -> 2 4 6
