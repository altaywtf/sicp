#lang racket

(require "./ch-2.4.2-tagged-data.rkt")
(require "./ch-2.4.3-lookup-table.rkt")
(require "./ch-2.5.2-coercion-lookup-table.rkt")

(provide apply-generic)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (arg1 (car args))
                    (arg2 (cdr args)))

                (if (eq? type1 type2)
                    (error "No method defined for this type: " type1)
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))

                      (cond ((t1->t2)
                             (apply-generic op (t1->t2 arg1) arg2))
                            ((t2->t1)
                             (apply-generic op arg2 (t2->t1 arg2)))
                            (else
                             (error "No method for these types: " (list op type-tags)))))))

              (error "No method for these types: " (list op type-tags)))))))
