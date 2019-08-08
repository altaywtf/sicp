#lang sicp

; ex-2.37: matrix algebra
; - vectors are represented as sequence of numbers
; - matrices are represented as sequences of vectors

; the internal implementation of map procedure from sicp is used
; (map + (list 1 2 3) (list 4 5 6) ...) -> (5 7 9)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


; example vector - matrix definitions
(define v1 (list 1 2 3 4))
(define v2 (list 4 5 6 6))
(define v3 (list 6 7 8 9))

(define m1 (list v1 v2 v3))


; matrix-algebra
; dot-product
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; (matrix-*-vector m v) -> returns the vector t where ti = Σj mij.vj
(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v)) m))

(matrix-*-vector m1 v1)

; (transpose m) -> returns the matrix n where nij = mji
(define (transpose m)
  (accumulate-n cons nil m))

(transpose m1)

; (matrix-*-matrix m n) -> return the matrix p where pij = Σk mik . nkj
(define (matrix-*-matrix m n)
  (let ((col (transpose n)))
    (map (lambda (m-row) (matrix-*-vector col m-row)) m)))

(matrix-*-matrix m1 (transpose m1))

