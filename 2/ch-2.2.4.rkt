#lang sicp

; ch-2.2.4: a picture language

(define (compose f g)
  (lambda (x) (f (g x))))

(define (for-each fn items)
  (if (null? items)
      (newline)
      (let ()
        (fn (car items))
        (for-each fn (cdr items)))))

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


; vector procedures from ex-2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v1)
  (make-vect
   (* s (xcor-vect v1))
   (* s (ycor-vect v1))))


; frame procedures from ex-2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))

(define (edge1-frame f) (cadr f))

(define (edge2-frame f) (caddr f))


; frame-coordinate-mapper
(define (frame-coord-map frame)
  (lambda (vector)
    (add-vect
     (origin-frame frame)
     (add-vect
      (scale-vect (xcor-vect vector) (edge1-frame frame))
      (scale-vect (ycor-vect vector) (edge2-frame frame))))))


; segment procedures from ex-2.48
(define (make-segment start-vector end-vector)
  (cons start-vector end-vector))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


; dummy draw-line
(define (draw-line start-v end-v)
  (display "line from ")
  (display start-v)
  (display " to ")
  (display end-v)
  (newline))


; segments -> painter
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
