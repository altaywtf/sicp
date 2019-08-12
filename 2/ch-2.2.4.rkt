#lang sicp

; ch-2.2.4

(define (compose f g)
  (lambda (x) (f (g x))))

; flip
(define (flip-vert painter) (string-append "f-v-" painter))
(define (flip-horiz painter) (string-append "f-h-" painter))

; rotate180
(define (rotate180)
  (compose flip-vert flip-horiz))


; beside
(define (beside p1 p2) (string-append "|" p1 "|" p2 "|"))

; below
(define (below p1 p2) (string-append p1 " / " p2))


; split
(define (split splitter placer)        
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split splitter placer) painter (- n 1))))
          (splitter painter (placer smaller smaller))))))

; right-split
(define right-split (split beside below))

; up-split
(define up-split (split below beside))

; corner-split
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1))) (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside
           (below top-left painter)
           (below corner bottom-right))))))


; higher-order-procedure for abstracting square-limit and flipped pairs
(define (square-of-four top-left top-right bottom-left bottom-right)
  (lambda (painter)
    (let ((top (beside (top-left painter) (top-right painter)))
          (bottom (beside (bottom-left painter) (bottom-right painter))))
      (below top bottom))))


(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert identity flip-vert)))
    (combine4 painter)))


(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
