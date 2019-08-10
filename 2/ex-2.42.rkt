#lang sicp

; ex-2.42: eight-queens puzzle

; helpers
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap op sequence)
  (accumulate append nil (map op sequence)))

(define (filter predictate sequence)
  (cond ((null? sequence) nil)
        ((predictate (car sequence))
         (cons (car sequence) (filter predictate (cdr sequence))))
        (else
         (filter predictate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define empty-board nil)

(define (adjoin-position row column rest-of-queens)
  (cons row rest-of-queens))

(define (safe? column positions)  
  (define (safe-position? row prev-row prev-col)
    (not (or (= row prev-row)
             (= (abs (- row prev-row))
                (abs (- column prev-col))))))
 
  (define (iter row prev-col prev-positions)
    (if (null? prev-positions)
        #t
        (and
         (safe-position? row (car prev-positions) prev-col)
         (iter row (- prev-col 1) (cdr prev-positions)))))
  
  (if (null? positions)
      #t
      (iter (car positions) (- column 1) (cdr positions))))

  
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  
  (queen-cols board-size))


(queens 1) ; ((1))
(queens 4) ; ((3 1 4 2) (2 4 1 3))
