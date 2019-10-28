#lang sicp

; ex-3.22: queue representation with local state

(define (make-queue)
  (let ((front-ptr '()) (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))

    (define (set-front-ptr! item)
      (set! front-ptr item))

    (define (set-rear-ptr! item)
      (set! rear-ptr item))

    (define (print-queue)
      front-ptr)

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (print-queue))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               (print-queue)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set-front-ptr! (cdr front-ptr))
             (print-queue))))

    (define (dispatch m)
      (cond ((eq? m 'insert) insert-queue!)
            ((eq? m 'delete) delete-queue!)
            ((eq? m 'empty?) empty-queue?)
            (else (error ":D"))))

    dispatch))


; example
(define q1 (make-queue))

((q1 'insert) 'a) ; (a)
((q1 'insert) 'b) ; (a b)
((q1 'insert) 'c) ; (a b c)

((q1 'delete)) ; (b c)
((q1 'delete)) ; (c)
((q1 'delete)) ; ()

((q1 'empty?)) ; #t
((q1 'delete)) ; error!
