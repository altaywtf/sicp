#lang racket
(require "./ch-2.3.4.rkt")

; ex-2.68: encode sample message

; encoding
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

; test
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))

(encode '(A D A B B C A) sample-tree) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)
(encode '(A A) sample-tree)           ; (0 0)
(encode '(D B C A) sample-tree)       ; (1 1 0 1 0 1 1 1 0)
(encode '(E) sample-tree)             ; error

