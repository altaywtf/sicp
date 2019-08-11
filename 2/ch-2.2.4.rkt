#lang sicp

; ch-2.2.4 -> example: a picture language

(define (display-wave wave)
  (newline)
  (display wave)
  (newline))

(define wave "w")

; flip
(define (flip-vert painter) (string-append "f-v-" painter))
(define (flip-horiz painter) (string-append "f-h-" painter))

; beside
(define (beside p1 p2) (string-append "|" p1 "|" p2 "|"))

; below
(define (below p1 p2) (string-append p1 " / " p2))

; flipped-pairs
(define (flipped-pairs painter)
  (let ((flipped-painter (beside painter (flip-vert painter))))
    (below flipped-painter flipped-painter)))

; right-split
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

; up-split
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below (beside smaller smaller) painter))))

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

; square-limit
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside quarter quarter)))
      (below (flip-vert half) half))))

(square-limit wave 1)
