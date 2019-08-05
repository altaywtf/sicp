#lang sicp

; ex-2.29: binary mobile

(define (make-mobile left-branch right-branch)
  (list left-branch right-branch))

(define (make-branch length structure)
  (list length structure))


; ex-2.29a: selectors for branches of a mobile
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))


; ex-2.29b: total weight of a mobile
(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else
         (+ (total-weight (branch-structure (left-branch mobile)))
            (total-weight (branch-structure (right-branch mobile)))))))

(define m1
  (make-mobile
   (make-branch 2 3)
   (make-branch 2 3)))
(total-weight m1) ; 6

(define m2
  (make-mobile
   (make-branch 2 3)
   (make-branch 2 m1)))
(total-weight m2) ; 9

(define m3
  (make-mobile
   (make-branch 4 6)
   (make-branch 2 (make-mobile (make-branch 5 8) (make-branch 10 4)))))
(total-weight m3) ; 18


; ex-2.29c: balanced?
(define (is-balanced? mobile)  
  (define (branch-torque branch)
    (* (branch-length branch) (total-weight (branch-structure branch))))
  
  (cond ((null? mobile) #t)
        ((not (pair? mobile)) #t)
        (else
         (and
          (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile)))
          (is-balanced? (branch-structure (left-branch mobile)))
          (is-balanced? (branch-structure (right-branch mobile)))))))

(is-balanced? m1) ; #t
(is-balanced? m2) ; #f
(is-balanced? m3) ; #t


; ex-2.29d
; just updating secondary element selectors, right-branch and branch-structure from (car (cdr list))) to (cdr pair) is required
