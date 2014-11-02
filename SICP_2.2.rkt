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

(define start (make-point -1 2))
(define end (make-point 3 -6))
(define line (make-segment start end))
(define m (midpoint-segment line))
(print-point m)