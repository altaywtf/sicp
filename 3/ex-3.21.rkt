#lang sicp

; ex-3.21: queue representation and printer

(define (make-queue)
  (cons '() '()))

(define (front-ptr queue)
  (car queue))

(define (rear-ptr queue)
  (cdr queue))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


;;; ben bitdiddle's overhaul
(define q1 (make-queue))

(insert-queue! q1 'a)
; ((a) a)

(insert-queue! q1 'b)
; ((a b) b)

(delete-queue! q1)
; ((b) b)

(delete-queue! q1)
; (() b)

; it still has b since we don't clean-up the rear-ptr in delete-queue! operation
; if we refill the queue, insert-queue!'s empty-queue? branch will reset the rear-ptr as well
(insert-queue! q1 'c)
; ((c) c)


;;; proper queue printer
(define (print-queue queue)
  (display (front-ptr queue))
  (newline))

(print-queue q1)
; (c)

(insert-queue! q1 'x)
(print-queue q1)
; (c x)

(delete-queue! q1)
(delete-queue! q1)
(print-queue q1)
; ()
