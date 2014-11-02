#lang racket

;let's say we represent a rectangle by two opposite corners
;knowing these two points we can calculate the width and the hight
(define make-rect
  (lambda (a b)
    (cons a b)))

(define (height rect)
  (abs (- (y-point (car rect)) (y-point (cdr rect)))))

(define (width rect)
  (abs (- (x-point (car rect)) (x-point (cdr rect)))))

(define (perimeter-rect rect)
  (+ (* 2 (height rect)) (* 2 (width rect))))

(define (area-rect rect)
  (* (height rect) (width rect)))

(define other-make-rect
  (lambda (a x y)
    (cons a (cons x y))))

(define (other-height rect)
  (car (cdr rect)))

(define (other-width rect)
  (cdr (cdr rect)))
  