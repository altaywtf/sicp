#lang racket
(require sicp-pict)

; ex-2.49: use-segments->painter to draw abcd

; a. the painter that draws the outline of the designated frame
(define segments-a
  (list
   (make-segment (make-vect 0 0) (make-vect 1 0))
   (make-segment (make-vect 0 0) (make-vect 0 1))
   (make-segment (make-vect 1 1) (make-vect 1 0))
   (make-segment (make-vect 1 1) (make-vect 0 1))))

(paint (segments->painter segments-a))


; b. the painter that draws x
(define segments-b
  (list
   (make-segment (make-vect 0 0) (make-vect 1 1))
   (make-segment (make-vect 0 1) (make-vect 1 0))))

(paint (segments->painter segments-b))

; c. the painter that draws a diamond shape by connecting the midpoints of the frame
(define segments-c
  (list
   (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
   (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
   (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
   (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))

(paint (segments->painter segments-c))
