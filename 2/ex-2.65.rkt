#lang sicp

; ex-2.65: implement o(n) union-set and intersection-set for balanced trees

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))


; tree->list2 from ex-2.62
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if
   (= n 0) (cons '() elts)
   (let ((left-size (quotient (- n 1) 2)))

     (let ((left-result (partial-tree elts left-size)))

       (let ((left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ 1 left-size))))

         (let ((middle-point (car non-left-elts))
               (right-result (partial-tree (cdr non-left-elts) right-size)))

           (let ((right-tree (car right-result))
                 (remaining-elts (cdr right-result)))

             (cons
              (make-tree middle-point left-tree right-tree)
              remaining-elts))))))))

; tree->list2 from ex-2.63
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  
  (copy-to-list tree '()))


; sample
(define my-set-1 (list->tree (list 1 2 3)))
(define my-set-2 (list->tree (list 2 3 4)))


; union-set
(define (union-set set-1 set-2)
  
  ; union-set for ordered lists, from ex-2.62
  (define (union-set-list list-1 list-2)
    (cond
      ((null? list-1) list-2)
      ((null? list-2) list-1)
      (else
       (let ((x1 (car list-1)) (x2 (car list-2)))
         (cond
           ((= x1 x2)
            (cons x1 (union-set-list (cdr list-1) (cdr list-2))))
           ((< x1 x2)
            (cons x1 (union-set-list (cdr list-1) list-2)))
           (else
            (cons x2 (union-set-list list-1 (cdr list-2)))))))))

  (list->tree (union-set-list (tree->list set-1) (tree->list set-2))))


(union-set my-set-1 my-set-2)


; intersection-set
(define (intersection-set set-1 set-2)

  ; intersection-set for ordered lists
  (define (intersection-set-list list-1 list-2)
    (if
     (or (null? list-1) (null? list-2)) '()
     (let ((x1 (car list-1)) (x2 (car list-2)))
       (cond
         ((= x1 x2)
          (cons x1 (intersection-set-list (cdr list-1) (cdr list-2))))
         ((< x1 x2)
          (intersection-set-list (cdr list-1) list-2))
         (else
          (intersection-set-list list-1 (cdr list-2)))))))

    (list->tree (intersection-set-list (tree->list set-1) (tree->list set-2))))

(intersection-set my-set-1 my-set-2)
