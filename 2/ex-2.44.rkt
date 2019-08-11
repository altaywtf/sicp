#lang sicp

; ex-2.44: implement up-split to be used in corner-split

(define (display-wave wave)
  (newline)
  (display wave)
  (newline))

(define wave "w")

; flip-vert
(define (flip-vert wave) (string-append "flip-v-" wave))

; beside
(define (beside wave1 wave2) (string-append "|" wave1 "|" wave2 "|"))

; below
(define (below wave1 wave2) (string-append wave1 " / " wave2))

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

(corner-split wave 1)
