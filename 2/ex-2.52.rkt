#lang racket
(require sicp-pict)

; ex-2.52
; a. add some segments to the wave
; b. change the pattern constructed by corner split
; c. modify the version of square-limit that uses square-of-four to assemble a different patterns


; a. wave with smile :D
(define wave 
  (segments->painter
   (list
    (make-segment (make-vect .45 .85) (make-vect .46 .85)) ; eye right
    (make-segment (make-vect .55 .85) (make-vect .54 .85)) ; eye left
    (make-segment (make-vect .45 .75) (make-vect .55 .75)) ; lip top
    (make-segment (make-vect .47 .70) (make-vect .53 .70)) ; lip bottom
    (make-segment (make-vect .47 .70) (make-vect .45 .75))
    (make-segment (make-vect .53 .70) (make-vect .55 .75))
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


; b. modify corner split

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
      (let ((top-left (up-split painter n))
            (top-right (corner-split painter (- n 1)))
            (bottom-left painter)
            (bottom-right (right-split painter n)))
        (below
         (beside bottom-left bottom-right)
         (beside top-left top-right)))))

(paint (corner-split wave 2))

; higher-order-procedure for abstracting square-limit and flipped pairs
(define (square-of-four top-left top-right bottom-left bottom-right)
  (lambda (painter)
    (let ((top (beside (top-left painter) (top-right painter)))
          (bottom (beside (bottom-left painter) (bottom-right painter))))
      (below top bottom))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert identity rotate180 flip-horiz)))
    (combine4 (corner-split painter n))))

(paint (square-limit einstein 2))
