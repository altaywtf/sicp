#lang racket
(require "./ch-2.3.4.rkt")

; ex-2.72
; - o(n) for encode (defined in ex-2.68)
; - # of steps needed for most and least frequent symbols in ex-2.71

; encoding (ex-2.68)
(define (encode-symbol symbol tree)
  (define (element-of-set? x set)
    (cond
      ((null? set) false)
      ((eq? (car set) x) true)
      (else (element-of-set? x (cdr set)))))
  
  (define (search-symbol symbol tree)
    (cond
     ((leaf? tree) '())
     ((element-of-set? symbol (symbols (left-branch tree)))
      (cons '0 (encode-symbol symbol (left-branch tree))))
     (else
      (cons '1 (encode-symbol symbol (right-branch tree))))))

  (if (element-of-set? symbol (symbols tree))
      (search-symbol symbol tree)
      (error "bad symbol -> " symbol)))

(define (encode message tree)
  (if
   (null? message)
   '()
   (append (encode-symbol (car message) tree)
           (encode (cdr message) tree))))

; encode-symbol -> n * logn (branches)
; encode -> n * encode-symbol

; for most frequent -> search-symbol will just run once
; for least frequent -> search-symbol will run n