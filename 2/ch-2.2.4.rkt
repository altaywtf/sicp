#lang sicp

; ch-2.2.4 -> example: a picture language

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


; test: wave2 and wave4
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(display-wave wave2)
(display-wave wave4)


; flipped-pairs
(define (flipped-pairs painter)
  (let ((flipped-painter (beside painter (flip-vert painter))))
    (below flipped-painter flipped-painter)))

; test: wave4 with flipped-pairs from wave
(display-wave (flipped-pairs wave))


; right-split
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

