#lang sicp

; ex-2.30: square-tree (analogous to square-list in ex 2.21) with map-tree and plain implementation

(define (square x) (* x x))

; map from ex-2.21
(define (map fn items)
  (if (null? items)
      nil
      (cons (fn (car items)) (map fn (cdr items)))))

(define (square-tree-map tree)
  (define (fn sub-tree)
    (if (not (pair? sub-tree))
        (square sub-tree)
        (square-tree-map sub-tree)))

  (map fn tree))


; generic map-tree
(define (map-tree fn tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (fn tree))
        (else
         (cons (map-tree fn (car tree))
               (map-tree fn (cdr tree))))))

(define (square-tree tree) (map-tree square tree))


; test
(define tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(square-tree tree)
(square-tree-map tree)
