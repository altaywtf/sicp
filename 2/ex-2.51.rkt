#lang racket
(require sicp-pict)

#|
 all the methods are actually defined sicp-pict
 this would make it easier to test the actual output
 while dealing with the remaining exercises
|#

; ex-2.51: define below with
; a. transformation
; b. using beside + other transformers like rotate

(define (below-transform painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
           (transform-painter painter1
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0)))
          (paint-bottom
           (transform-painter painter2
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(paint (below-transform einstein einstein))


(define (below-beside-rotate painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
  
(paint (below-beside-rotate einstein einstein))
