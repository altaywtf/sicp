#lang sicp

(define (pascal row col)
  (cond
    ((< row col) 0)
    ((or (= col 1) (= row col)) 1)
    (else
     (+
      (pascal (- row 1) col)
      (pascal (- row 1) (- col 1))))))

(pascal 3 2)