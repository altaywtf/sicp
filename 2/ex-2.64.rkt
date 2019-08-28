#lang sicp

; ex-2.64: explain how list->tree works and order of growth

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if
   (= n 0) (cons '() elts)
   (let ((left-size (quotient (- n 1) 2)))

     (let ((left-result (partial-tree elts left-size)))

       (let ((left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ 1 left-size))))

         (let ((middle-point (car non-left-elts))
               (right-result (partial-tree (cdr non-left-elts) right-size)))

           (let ((right-tree (car right-result))
                 (remaining-elts (cdr right-result)))

             (cons
              (make-tree middle-point left-tree right-tree)
              remaining-elts))))))))

(define test-list (list->tree (list 1 2 3 4 5)))
test-list

; a) list->tree calls partial-tree with all the elements + length of elements in the list then partial-tree
; - estimates a list-size for the left branch (n-1)/2
; - recursively creates a balanced tree for the left branch
; - calculates the # of elements to be placed in the right-size
; - list->tree calls it with all the elements so in this case [all of the remaining - 1 (because middle point will be entry)]
; - picks the first element from remaning as the middle-point
; - recursively creates a balanced tree for the right branch
; - returns a pair of balanced tree and unused elements (in this case nil because list->tree calls it with all the elements)

; b) order-of-growth is dependent on partial-tree
; - visits all the elements -> o(n)
; - uses cons -> o(1)
; - o(n)

