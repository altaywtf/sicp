#lang sicp

; ex-3.16: incorrect count-pairs

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+
       1
       (count-pairs (car x))
       (count-pairs (cdr x)))))


;
; t3--->[*][*]--->[*][*]--->[*][/]
;        |         |         |
;        v         v         v
;       "a"       "b"       "c"

(define t3 '(a b c))
(count-pairs t3)

;
; t4--->[*][\]
;        |    
;        v    
;      [*][*]
;      |___|
;      |
;      v   
;    [*][/]
;     |
;     v
;    "x"
;
(define x '(x))
(define y (cons x x))
(define t4 (list y))
(count-pairs t4)

;
; t7--->[*][*]
;        |__|
;        |
;        v      
;      [*][*]
;      |___|
;      |
;      v   
;    [*][/]
;     |
;     v
;    "k"
;
(define k '(k))
(define l (cons k k))
(define t7 (cons l l))
(count-pairs t7)

;
; inf
; 
(define tinf '(a b c))
(set-cdr! (cdr (cdr tinf)) tinf)
; (count-pairs tinf)

