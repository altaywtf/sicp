#lang racket
(provide (all-defined-out))

; ch-2.3.4: representing huffman encoding trees

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


; adjoin
(define (adjoin-set leaf set)
  (cond
    ((null? set) (list leaf))
    ((< (weight leaf) (weight (car set))) (cons leaf set))
    (else (cons (car set) (adjoin-set leaf (cdr set))))))


; make-leaf-set -> takes pairs and creates ordered set of leaves
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair) (cadr pair))
         (make-leaf-set (cdr pairs))))))


; encoding (ex-2.68)
(define (encode-symbol symbol tree)
  (define (element-of-set? x set)
    (cond
      ((null? set) false)
      ((eq? (car set) x) true)
      (else (element-of-set? x (cdr set)))))
  
  (define (search-symbol t result)
    (cond
     ((leaf? t)
      result)
     ((eq? (symbol-leaf (left-branch t)) symbol)
      (cons '0 result))
     (else
      (cons '1 (search-symbol (right-branch t) result)))))

  (if (element-of-set? symbol (symbols tree))
      (search-symbol tree '())
      (error "bad symbol -> " symbol)))

(define (encode message tree)
  (if
   (null? message)
   '()
   (append (encode-symbol (car message) tree)
           (encode (cdr message) tree))))


; generate-huffman-tree from pairs (ex-2.69)
(define (successive-merge leaf-set)
  (if
   (= (length leaf-set) 1)
   (car leaf-set)
   (successive-merge
    (adjoin-set
     (make-code-tree (car leaf-set) (cadr leaf-set))
     (cddr leaf-set)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

