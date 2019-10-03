#lang racket

(require "./ch-2.4.2-tagged-data.rkt")
(require "./ch-2.4.3-lookup-table.rkt")
(require "./ch-2.5.2-coercion-lookup-table.rkt")

(define (is-all-eq? items)
  (if (= (length items) 1)
      #t
      (and
       (eq? (car items) (cadr items))
       (is-all-eq? (cdr items)))))

(define (find-coercion types)
  (define (find-coercion-iter type rest result)    
    (if (null? rest)
        result
        (let ((coercion (get-coercion (car rest) type)))
          (if coercion
              (find-coercion-iter type (cdr rest) coercion)
              #f))))
  (find-coercion-iter (car types) (cdr types) #f))
    

(provide apply-generic)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (is-all-eq? type-tags)
              (error "No method defined for this type: " (car type-tags))
              (let ((coercion (find-coercion type-tags)))
                (if coercion
                    (apply-generic op (car args) (coercion (cadr args)))
                    (error "No method for these types: " (list op type-tags)))))))))
              
