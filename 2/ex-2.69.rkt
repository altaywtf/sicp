#lang racket
(require "./ch-2.3.4.rkt")

; ex-2.69: generate-huffman-tree from pairs -> complete successive merge to use make-code-tree
(define (successive-merge leaf-set)
  (display leaf-set)
  (newline)
  (if
   (null? (cdr leaf-set))
   (car leaf-set)
   (successive-merge
    (adjoin-set
     (make-code-tree (cadr leaf-set) (car leaf-set))
     (cddr leaf-set)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


; test
(define sample-pairs '((A 4) (B 2) (C 1) (D 1)))

(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'C 1)
     (make-leaf 'D 1)))))

(equal? (generate-huffman-tree sample-pairs) sample-tree)
