#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;transforming an combining painters
;operations on painters do not need to know how a painter works
;instead, they manipulate frames
;they you use the original painter, but a transformed frame

;nevertheless, the procedure you use is called transform-painter
;its arguments are a painter and info on how to transform a frame
;it outputs a new painter
;when you use this new painter on a frame, it transforms the frame and then uses the original painter
;how is info about frame transformation represented?
;as vectors specifying the positions of corners of the new frame
;the first vecor specifies the new frame's origin
;the other two specify the ends of the edge vectors of the new frame

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter (make-vect 0.5 0.5) (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter (make-vect 1.0 0.0) (make-vect 1.0 1.0) (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter (make-vect 0.0 0.0) (make-vect 0.65 0.35) (make-vect 0.35 0.65)))                                                 
;seems like edge1 is the horizontal one
               
;combining painters also happens through frame transformation
;e.g. besides takes two painters, transforms them to be painted in one half of the frame each, and returns a new, compound painter

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter painter1
                                         (make-vect 0.0 0.0)
                                         split-point
                                         (make-vect 0.0 1.0)))
          (paint-right (transform-painter painter2
                                          split-point
                                          (make-vect 1.0 0.0)
                                          (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;so, beside p1 p2 outputs a function which requires a frame
;when you feed it the frame, it transforms it twice, and draws painters in the two halves of the frame

;Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter (make-vect 0.0 1.0) (make-vect 1.0 1.0) (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter (make-vect 1.0 1.0) (make-vect 0.0 1.0) (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter (make-vect 0.0 1.0) (make-vect 0.0 0.0) (make-vect 1.0 1.0)))

;Exercise 2.51
(define (below painter1 painter2)
  (let ((spli-point (make-vect 0.0 0.5)))
    (let ((paint-bottom (transform-painter painter1
                                           (make-vect 0.0 0.0)
                                           (make-vect 1.0 0.0)
                                           split-point))
          (paint-top (transform-painter painter2
                                        split-point
                                        (make-vect 1.0 0.5)
                                        (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-right-frame)))))

;define below in terms of besides and rotations
;apply besides and rotation by 270
;apply rotation by 90 to the whole frame

(define (below2 painter1 painter2)
  (let ((rotated1 (rotate270 painter1))
        (rotated2 (rotate270 painter2)))
    (rotate90 (besides rotated1 rotated2))))
                                         