#lang sicp

; ex-2.63: compare two list->tree converters
; - a) do they produce same results for every tree?
; - b) compare order of growth for BALANCED TREES

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (adjoin-set x set)
  (cond
    ((null? set) (make-tree x '() '()))
    ((= x (entry set)) set)
    ((< x (entry set))
     (make-tree
      (entry set)
      (adjoin-set x (left-branch set))
      (right-branch set)))
    ((> x (entry set))
     (make-tree
      (entry set)
      (left-branch set)
      (adjoin-set x (right-branch set))))))

; (1 2 3)
(define my-balanced-set
  (make-tree
   2
   (adjoin-set 1 nil)
   (adjoin-set 3 nil)))

; (1 2 3 4 5 6 7) with consequetive adjoins (fig 2.17)
(define my-messy-set
  (adjoin-set 7 (adjoin-set 6 (adjoin-set 5 (adjoin-set 4 (adjoin-set 3 (adjoin-set 2 (adjoin-set 1 nil))))))))


; tree->list1
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 (left-branch tree))
       (cons (entry tree) (tree->list-1 (right-branch tree))))))


; tree->list2
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  
  (copy-to-list tree '()))


; a)
(tree->list-1 my-balanced-set) ; (1 2 3) 
(tree->list-2 my-balanced-set) ; (1 2 3)

(tree->list-1 my-messy-set) ; (1 2 3 4 5 6 7)
(tree->list-2 my-messy-set) ; (1 2 3 4 5 6 7)

; recursive pattern in both of the prodecures look similar, and also the outputs.


; b)
; cons   -> has a constant factor of growth
; append -> has a proportional factor of growth based on the left-branch

; tree-list-1
; uses append but the tree is balanced so the length of left branch will be equal to the right worst case
; which will introduce o(logn) to the growth -> o(n logn)

; tree-list-2
; uses cons -> o(n)
