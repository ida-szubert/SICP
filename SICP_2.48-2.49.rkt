#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;Painter
;it's a procedure which, given a frame, draws an image shifted and scaled to fit the frame
;how are painters implemented?
;e.g having a procedure draw-line taking 2 points as arguments, we can create a painter for line drawing

;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) (start-segment segment))
;        ((frame-coord-map frame) (end-segment segment))))
;     segment-list)))

;segments are given using coordinates with respect to the unit square
;so, the painter transforms end-points of each segment and draws a line between the transformed points
;any procedure can serve as a painter, provided that it takes a frame as argument and draws something scaled to fit the frame

;Exercise 2.48
;a directed line segment can be represented by 2 vectors - from origin to the start-point and from origin to the end-point
  
;(define (make-segment vs ve)
;  (cons vs ve))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

;Exercise 2.49
;segments->painter requires a list of segments as its imput
;we have to define such segments that will prodece the require painter when fed into segments->painter
;a.
(define outline-segments
     (list (make-segment (make-vect 0.0 0.0)
                         (make-vect 0.0 0.99))
           (make-segment (make-vect 0.0 0.0)
                         (make-vect 0.99 0.0))
           (make-segment (make-vect 0.99 0.0)
                         (make-vect 0.99 0.99))
           (make-segment (make-vect 0.0 0.99)
                         (make-vect 0.99 0.99))))

;if the vectors end or start at 1 instead of 0.99, the upper and right boundaries are not visible

(define outline
  (segments->painter outline-segments))

(paint outline)

;b. draw an x
(define diagonals-segments
  (list (make-segment (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
        (make-segment (make-vect 1.0 0.0)
                      (make-vect 0.0 1.0))))

(define draw-x
  (segments->painter diagonals-segments))

(paint draw-x)

;c. diamond shape
(define diamond-segments
  (list (make-segment (make-vect 0.5 0.0)
                      (make-vect 1.0 0.5))
        (make-segment (make-vect 0.5 0.0)
                      (make-vect 0.0 0.5))
        (make-segment (make-vect 1.0 0.5)
                      (make-vect 0.5 1.0))
        (make-segment (make-vect 0.0 0.5)
                      (make-vect 0.5 1.0))))
;it's not perfect, cause when you use 1.0 as a coordinate, lines in the upper and right corner do not meet
;but if yu use 0.99, then the lines are not straight

(define diamond
  (segments->painter diamond-segments))

(paint diamond)
              
;d. wave painter
;either there's a way of drawing curves or it would take a zillion segments to define the wave painter



