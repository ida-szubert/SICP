#lang racket
;Exercise 2.12
(define (make-interval a b)
  (cons a b))
(define (lower-bound x)
  (car x))
(define (upper-bound x)
  (cdr x))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define y (make-interval 6.12 7.48))
(center y)
(width y)

(define (make-center-percent c p)
  (make-interval  (- c (* (/ p 100) c)) (+ c (* (/ p 100) c))))

(define (percent i)
  (* (/ (- (upper-bound i) (center i)) (center i)) 100))

(percent y)
              
;Exercise 2.13
(define (spans-zero? a)
  (and (negative? (lower-bound a)) (positive? (upper-bound a))))

(define mul-interval
  (lambda (x y)
    (if (or (spans-zero? x) (spans-zero? y))
        (let ((p1 (* (lower-bound x) (lower-bound y)))
              (p2 (* (lower-bound x) (upper-bound y)))
              (p3 (* (upper-bound x) (lower-bound y)))
              (p4 (* (upper-bound x) (upper-bound y))))
          (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))
        (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))))

;how can we approximate the p of the product of two intervals?
(define a (make-center-percent 2 1.5))
(define b (make-center-percent 3.5 1))

(define c (mul-interval a b))
(percent c); 2.49962
;seems like you can simply add up the uncertainty of a and b

(define d (make-center-percent 7.14 0.75))
(define e (make-center-percent 5.67 1.15))
(define f (mul-interval d e))

(percent d); 0.75000
(percent e); 1.14999
(percent f); 1.89983


