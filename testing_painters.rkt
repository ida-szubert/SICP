#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (new-corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (new-corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))
                  
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (new-square-limit painter n)
  (let ((combine4 (square-of-four rotate270 rotate180 identity rotate90)))
    (combine4 (corner-split painter n))))


(define diamond-segments
  (list (make-segment (make-vect 0.5 0.0)
                      (make-vect 1.0 0.5))
        (make-segment (make-vect 0.5 0.0)
                      (make-vect 0.0 0.5))
        (make-segment (make-vect 1.0 0.5)
                      (make-vect 0.5 1.0))
        (make-segment (make-vect 0.0 0.5)
                      (make-vect 0.5 1.0))
        (make-segment (make-vect 0.32 0.6)   ;left eye
                      (make-vect 0.37 0.65))
        (make-segment (make-vect 0.37 0.65)
                      (make-vect 0.42 0.6))
        (make-segment (make-vect 0.58 0.6)   ;right eye
                      (make-vect 0.63 0.65))
        (make-segment (make-vect 0.63 0.65)
                      (make-vect 0.68 0.6))
        (make-segment (make-vect 0.44 0.42)  ;smile
                      (make-vect 0.56 0.42))
        (make-segment (make-vect 0.44 0.42) 
                      (make-vect 0.44 0.45))
        (make-segment (make-vect 0.56 0.42)
                      (make-vect 0.56 0.45))))

(define diamond
  (segments->painter diamond-segments))

(paint diamond)
;(paint (square-limit diamond 4))
;(paint (new-square-limit diamond 4))
;(paint (new-corner-split diamond 4))
;(paint (corner-split diamond 4))

(define himmeli-segments
  (list (make-segment (make-vect 0.2 0.7)
                      (make-vect 0.45 0.0))
        (make-segment (make-vect 0.45 0.55)
                      (make-vect 0.45 0.0))
        (make-segment (make-vect 0.6 0.85)
                      (make-vect 0.45 0.0))
        (make-segment (make-vect 0.8 0.7)
                      (make-vect 0.45 0.0)) ;lower pyramid
        (make-segment (make-vect 0.5 1.0)
                      (make-vect 0.2 0.7))
        (make-segment (make-vect 0.5 1.0)
                      (make-vect 0.45 0.55))
        (make-segment (make-vect 0.5 1.0)
                      (make-vect 0.6 0.85))
        (make-segment (make-vect 0.5 1.0)
                      (make-vect 0.8 0.7))  ;upper pyramid
        (make-segment (make-vect 0.2 0.7)
                      (make-vect 0.45 0.55))
        (make-segment (make-vect 0.45 0.55) 
                      (make-vect 0.8 0.7))
        (make-segment (make-vect 0.8 0.7)
                      (make-vect 0.6 0.85))
        (make-segment (make-vect 0.6 0.85) 
                      (make-vect 0.2 0.7)))) ;base

(define himmeli
  (segments->painter himmeli-segments))

(paint himmeli)
;(paint (square-limit himmeli 4))

(define (transform-painterr painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))


(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s) (* (ycor-vect v) s)))

(define (tilt-to-the-right painter)
   (transform-painterr painter (make-vect 0.0 0.5) (make-vect 0.5 0.0) (make-vect 0.5 1.0))) 

(define (shrink-to-upper-right painter)
  (transform-painterr painter (make-vect 0.5 0.5) (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

(define (squash-inwards painter)
  (transform-painterr painter (make-vect 0.0 0.0) (make-vect 0.65 0.35) (make-vect 0.35 0.65)))                                                 
;seems like edge1 is the horizontal one

;(paint (shrink-to-upper-right diamond))
;(paint (squash-inwards diamond))
(paint (tilt-to-the-right himmeli))
(paint (square-limit (tilt-to-the-right himmeli) 3))
(paint (square-limit (flip-vert (flip-horiz (tilt-to-the-right himmeli))) 3))
               
;combining painters also happens through frame transformation
;e.g. besides takes two painters, transforms them to be painted in one half of the frame each, and returns a new, compound painter

(define (beside1 painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painterr painter1
                                         (make-vect 0.0 0.0)
                                         split-point
                                         (make-vect 0.0 1.0)))
          (paint-right (transform-painterr painter2
                                          split-point
                                          (make-vect 1.0 0.0)
                                          (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;so, beside p1 p2 outputs a function which requires a frame
;when you feed it the frame, it transforms it twice, and draws painters in the two halves of the frame

;Exercise 2.50
(define (flip-horizontal painter)
  (transform-painterr painter (make-vect 0.0 1.0) (make-vect 1.0 1.0) (make-vect 0.0 0.0)))

(define (rotate-180 painter)
  (transform-painterr painter (make-vect 1.0 1.0) (make-vect 0.0 1.0) (make-vect 1.0 0.0)))

(define (rotate-270 painter)
  (transform-painterr painter (make-vect 0.0 1.0) (make-vect 0.0 0.0) (make-vect 1.0 1.0)))

;Exercise 2.51
(define (below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom (transform-painterr painter1
                                           (make-vect 0.0 0.0)
                                           (make-vect 1.0 0.0)
                                           split-point))
          (paint-top (transform-painterr painter2
                                        split-point
                                        (make-vect 1.0 0.5)
                                        (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

;define below in terms of besides and rotations
;apply besides and rotation by 270
;apply rotation by 90 to the whole frame

(define (below2 painter1 painter2)
  (let ((rotated1 (rotate-270 painter1))
        (rotated2 (rotate-270 painter2)))
    (rotate90 (beside1 rotated1 rotated2))))
                                         
;(paint (flip-horizontal einstein))
;(paint (rotate-180 einstein))
;(paint (rotate-270 einstein))
;(paint (below1 einstein einstein))