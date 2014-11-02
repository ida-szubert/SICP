#lang planet neil/sicp



(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

;Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
;--------------------------------------------------------

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

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;we can abstract patterns with higher-order procedures
;e.g fliped-pairs and square-limit both arrange 4 painters into a square pattern
;we can write a procedure which takes 4 operations, and each is applied to one copy of the painter

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (another-square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;Exercise 2.45
(define (split op1 op2)
  (define (help painter n)
    (if (= n 0)
        painter
        (let ((smaller (help painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  (help))

;Frames
;each frame is represented by 3 vectors
;one starts at some origin point of the screen and end at the origin point of the frame
;the other two specify the two edges of the frame
;since frames are parallelograms, that's enough

;with each frame we associate a fram coordinate map, used to shift and scale images to fit the frame
;the map transforms unit square into a frame by mapping the vecotr v = (x,y) to the sum of the three vecors describing the frame
;Origin + x * Edge1 + y * Edge2

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

;Exercise 2.46
(define (make-vect x y)
  (cons x y))
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

;Exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin2 frame)
  (car frame))
(define (edge1.2 frame)
  (cadr frame))
(define (edge2.2 frame)
  (cddr frame))









