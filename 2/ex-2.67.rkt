#lang sicp

; ex-2.67: decode sample message

; leaf {'leaf symbol weight}
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf leaf)
  (cadr leaf))

(define (weight-leaf leaf)
  (caddr leaf))

; code tree -> {left-branch, right-branch, symbols, weight}
(define (make-code-tree left-branch right-branch)
  (list
   left-branch
   right-branch
   (append
    (symbols left-branch)
    (symbols right-branch))
   (+
    (weight left-branch)
    (weight right-branch))))

; selectors
(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols object)
  (if (leaf? object)
      (list (symbol-leaf object))
      (caddr object)))

(define (weight object)
  (if (leaf? object)
      (weight-leaf object)
      (cadddr object)))

; decoding
(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= 0 bit) (left-branch branch))
          ((= 1 bit) (right-branch branch))
          (else (error "bad bit: " bit))))
  
  (define (decode-1-iter bits current-branch)
    (if
     (null? bits) '()
     (let ((next-branch (choose-branch (car bits) current-branch)))
       (if
        (leaf? next-branch)
        (cons (symbol-leaf next-branch) (decode-1-iter (cdr bits) tree))
        (decode-1-iter (cdr bits) next-branch)))))

  (decode-1-iter bits tree))


; solution
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))

(define sample-bits '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-bits sample-tree) ; (A D A B B C A)
