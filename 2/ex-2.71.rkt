#lang racket
(require "./ch-2.3.4.rkt")

; ex-2.71: huffman-trees with increasing symbol frequencies (2^(n-1))
; - sketch the tree for n=5
; - sketch the tree for n=10
; - how many bits are required to encode the most and least frequent symbols?

(define characters
  '(A B C D E F G H I J))

(define (generate-symbols n)
  (if (= 0 n)
      '()
      (cons
       (list (list-ref characters (- n 1)) (expt 2 (- n 1)))
       (generate-symbols (- n 1)))))


; tree for n=5
(define tree-5 (generate-huffman-tree (generate-symbols 5)))

(encode '(E) tree-5) ; -> (1)
(encode '(A) tree-5) ; -> (0 0 0 0)


; tree for n=10
(define tree-10 (generate-huffman-tree (generate-symbols 10)))

(encode '(J) tree-10) ; -> (1)
(encode '(A) tree-10) ; -> (0 0 0 0 0 0 0 0 0)


; based on the values above
; - least frequent symbol -> (n - 1) bits
; - most frequent symbol  -> 1 bits
