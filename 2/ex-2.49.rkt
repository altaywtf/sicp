#lang racket
(require sicp-pict)

; ex-2.49: use-segments->painter to define painters for given shapes

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

; d. wave :D (taken from -> http://community.schemewiki.org/?sicp-ex-2.49)
(define wave 
  (segments->painter
   (list 
    (make-segment (make-vect .25 0) (make-vect .35 .5)) 
    (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
    (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
    (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
    (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
    (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
    (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
    (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
    (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
    (make-segment (make-vect .35 .85) (make-vect .4 1)) 
    (make-segment (make-vect .4 1) (make-vect .6 1)) 
    (make-segment (make-vect .6 1) (make-vect .65 .85)) 
    (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
    (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
    (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
    (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
    (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
    (make-segment (make-vect .6 .45) (make-vect .75 0)) 
    (make-segment (make-vect .75 0) (make-vect .6 0)) 
    (make-segment (make-vect .6 0) (make-vect .5 .3)) 
    (make-segment (make-vect .5 .3) (make-vect .4 0)) 
    (make-segment (make-vect .4 0) (make-vect .25 0)))))

(paint wave)
