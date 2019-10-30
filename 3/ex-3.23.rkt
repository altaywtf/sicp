#lang sicp

; ex-3.23: implement dequeue

(define (make-dequeue)
  (cons nil nil))

(define (front-ptr dq)
  (car dq))

(define (rear-ptr dq)
  (cdr dq))

(define (front-dequeue dq)
  (caar (front-ptr dq)))

(define (rear-dequeue dq)
  (caar (rear-ptr dq)))

(define (set-front-ptr! dq item)
  (set-car! dq item))

(define (set-rear-ptr! dq item)
  (set-cdr! dq item))

(define (empty-dequeue? dq)
  (null? (front-ptr dq)))

(define (front-insert-dequeue! dq item)
  (let ((new-pair (cons (cons item nil) nil)))
    (cond ((empty-dequeue? dq)
           (set-front-ptr! dq new-pair)
           (set-rear-ptr! dq new-pair))
          (else
           (set-cdr! new-pair (front-ptr dq))
           (set-front-ptr! dq new-pair)))))

(define (front-delete-dequeue! dq)
  (cond ((empty-dequeue? dq)
         (error "front-delete called with an empty dequeue: " dq))
        (else
         (set-front-ptr! dq (cdr (front-ptr dq))))))

(define (rear-insert-dequeue! dq item)
  (let ((new-pair (cons (cons item (rear-ptr dq)) nil)))
    (cond ((empty-dequeue? dq)
           (set-front-ptr! dq new-pair)
           (set-rear-ptr! dq new-pair))
          (else
           (set-cdr! (rear-ptr dq) new-pair)
           (set-rear-ptr! dq new-pair)))))

(define (rear-delete-dequeue! dq)
  (cond ((empty-dequeue? dq)
         (error "rear-delete called with an epty dequeue: " dq))
        (else
         (set-rear-ptr! dq (cdar (rear-ptr dq)))
         (set-cdr! (rear-ptr dq) nil))))

(define (print-dequeue dq)
  (define (print-end) (display ")") (newline))
  (display "(")

  (let print-next ((next (front-ptr dq)))
    (cond ((null? next)
           (print-end))
          ((null? (cdr next))
           (display (caar next))
           (print-end))
          (else (display (caar next))
                (display " ")
                (print-next (cdr next))))))

; test
(define q1 (make-dequeue))

(front-insert-dequeue! q1 'a)
(front-insert-dequeue! q1 'b)
(front-insert-dequeue! q1 'c)

(print-dequeue q1) ; (c b a)

(front-delete-dequeue! q1)

(print-dequeue q1) ; (b a)

(rear-insert-dequeue! q1 'x)
(rear-insert-dequeue! q1 'y)

(print-dequeue q1) ; (b a x y)

(rear-delete-dequeue! q1)

(print-dequeue q1) ; (b a x)
