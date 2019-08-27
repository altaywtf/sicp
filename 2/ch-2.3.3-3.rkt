#lang sicp

; ch-2.3.2-3: sets as binary trees

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))


(define (element-of-set? x set)
  (cond
    ((null? set) false)
    ((= x (entry set)) true)
    ((< x (entry set))
     (element-of-set? x (left-branch set)))
    ((> x (entry set))
     (element-of-set? x (right-branch set)))))


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

(define my-set
  (make-tree
   2
   (make-tree 1 '() '())
   (make-tree 3 '() '())))

(element-of-set? 1 my-set)
(entry (right-branch (right-branch (adjoin-set 4 my-set))))
