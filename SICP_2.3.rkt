#lang racket

(define (make-point x y) (cons x y))
(define (x-point a) (car a))
(define (y-point a) (cdr a))

;let's say we represent a rectangle by two opposite corners
;knowing these two points we can calculate the width and the hight
(define make-rect
  (lambda (a b)
    (cons 'corners (cons a b))))

;another way would be to use width, height, and origin (e.g. bottom left corner)
(define other-make-rect
  (lambda (a x y)
    (cons 'origin (cons a (cons x y)))))


(define (width rect)
  (if (eq? (car rect) 'corners)
      (abs (- (x-point (cadr rect)) (x-point (cddr rect))))
      (cdddr rect)))

(define (height rect)
  (if (eq? (car rect) 'corners)
      (abs (- (y-point (cadr rect)) (y-point (cddr rect))))
      (caddr rect)))


(define (perimeter-rect rect)
  (+ (* 2 (height rect)) (* 2 (width rect))))

(define (area-rect rect)
  (* (height rect) (width rect)))


(define rectangle1
  (let ((a (make-point 1 1))
        (b (make-point 5 8)))
    (make-rect a b)))

(define rectangle2
  (let ((a (make-point 1 1)))
    (other-make-rect a 7 4)))

(perimeter-rect rectangle1)
(area-rect rectangle1)
(perimeter-rect rectangle2)
(area-rect rectangle2)
