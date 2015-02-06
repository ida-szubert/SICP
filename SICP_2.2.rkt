#lang racket
(define (make-segment start end) (cons start end))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (make-point x y) (cons x y))
(define (x-point a) (car a))
(define (y-point a) (cdr a))

(define (midpoint-segment s)
  (define (middle a b) (/ (+ a b) 2))
  (make-point (middle (x-point (start-segment s)) (x-point (end-segment s)))
              (middle (y-point (start-segment s)) (y-point (end-segment s)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define line
  (let ((start (make-point -1 2))
        (end (make-point 3 -6)))
    (make-segment start end)))

(print-point (midpoint-segment line))
