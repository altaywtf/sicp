#lang sicp

; ex-3.17: correct count-pairs that recognizes identical pairs
(define (make-counter)
  (define count-pairs
    (let ((seen-pairs '()))
      (lambda (x)
        (cond ((not (pair? x)) 0)
              ((memq x seen-pairs) 0)
              (else
               (set! seen-pairs (cons x seen-pairs))
               (+ (count-pairs (car x))
                  (count-pairs (cdr x))
                  1))))))
  count-pairs)


(define t3 '(a b c))
((make-counter) t3) ; -> 3

(define x '(x))
(define y (cons x x))
(define t4 (list y))
((make-counter) t4) ; -> 3

(define k '(k))
(define l (cons k k))
(define t7 (cons l l))
((make-counter) t7) ; -> 3

(define tinf '(a b c))
(set-cdr! (cdr (cdr tinf)) tinf)
((make-counter) tinf) ; -> 3
