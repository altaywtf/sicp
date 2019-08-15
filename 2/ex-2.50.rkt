#lang racket
(require sicp-pict)

#|
 all the methods are actually defined sicp-pict
 this would make it easier to test the actual output
 while dealing with the remaining exercises
|#

; ex-2.50: implement flip-horiz, rotate180 and rotate270

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(paint (flip-horiz einstein))


(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(paint (rotate180 einstein))


(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(paint (rotate270 einstein))
