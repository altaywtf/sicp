#lang sicp

; ex-3.24: local table with external same-key? predictate

(define (make-table same-key?)
  (let ((local-table (list '*table)))
    (define (assoc key records)
      (cond ((null? records)
             false)
            ((same-key? key (caar records))
             (car records))
            (else
             (assoc key (cdr records)))))

    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))

    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table (cons (cons key value) (cdr local-table))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else
             (error "unknown proc"))))

    dispatch))


; examples
(define t1 (make-table (lambda (x y) (eq? x y))))

((t1 'insert!) 'a 1)
((t1 'lookup) 'a) ; 1

((t1 'insert!) 2 'b)
((t1 'lookup) 2) ; 'b


; predictate with tolerance
(define t2 (make-table (lambda (x y) (<= (abs (- x y)) 1))))
((t2 'insert!) 1 1)
((t2 'lookup) 1) ; 1

((t2 'insert!) 2 2)
((t2 'lookup) 2) ; 2

((t2 'lookup) 1) ; 1
