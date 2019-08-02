#lang sicp

; ex-2.17: implement (last-pair list) procedure

(define my-list (list 23 72 149 37))

; list-ref procedure from ch 2.2.1
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))


; last-pair that uses list-ref
(define (last-pair-ref l)
  (list-ref l (- (length l) 1)))

(last-pair-ref my-list)


; last-pair-iter
(define (last-pair-iter l)
  (define (iter items i)
    (if (= i (- (length l) 1))
        (car items)
        (iter (cdr items) (+ i 1))))

  (iter l 0))

(last-pair-iter my-list)


; last-pair
(define (last-pair l)
  (let ((length-l (length l)))
    (cond ((= 0 length-l) nil)
          ((= 1 length-l) (car l))
          (else (last-pair (cdr l))))))

(last-pair my-list)
(last-pair (list))
