#lang racket
(require "./ch-2.3.4.rkt")

; ex-2.68: encode sample message

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))

(encode '(A D A B B C A) sample-tree) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)
(encode '(A A) sample-tree)           ; (0 0)
(encode '(D B C A) sample-tree)       ; (1 1 0 1 0 1 1 1 0)
(encode '(E) sample-tree)             ; error
