#lang racket

;parellel equivalent resistance of two restistors
;Rp = 1 / ((1/R1) + (1/R2))

;the system will be used for manipulating intervals (resistors have some known precision)

(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (car x))
(define (upper-bound x)
  (cdr x))

(define add-interval
  (lambda (x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y)))))

(define mul-interval
  (lambda (x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
         (p2 (* (lower-bound x) (upper-bound y)))
         (p3 (* (upper-bound x) (lower-bound y)))
         (p4 (* (upper-bound x) (upper-bound y))))
      (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))))

(define div-interval
  (lambda (x y)
    (mul-interval
     x
     (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))))

;Exercise 2.8
(define sub-interval
  (lambda (x y)
    (make-interval (- (lower-bound x) (lower-bound y))
                   (- (upper-bound x) (upper-bound y)))))

;Exercise 2.9
;the width of an interval is half of the difference between the lower and upper bounds
;show that the width of the sum of a and b depends only on the width of a and width of b

;width of a: (/ (- (upper-bound a) (lower-bound a)) 2)
;width of b: (/ (- (upper-bound b) (lower-bound b)) 2)

;width of a+b: (/ (- (upper-bound c) (lower-bound c)) 2)
;where c is ((+ (lower-bound a) (lower-bound b))
;            (+ (upper-bound a) (upper-bound b)))
;width of a+b: (/ (-  (+ (upper-bound a) (upper-bound b)) (+ (lower-bound a) (lower-bound b))) 2)
;because + and - are comutative, it's equivalent to
;(/ (+ (- (upper-bound a) (lower-bound a)) (- (upper-bound c) (lower-bound b))) 2)
;which is (/ (+ (width a) (width b)) 2)

;now, show that the width of the product of a and b doesn't depend only on the widths of a and b
;it doesn't, because the lower bound of a*b is not neccesarily a product of the lower bounds of a and b
;the same goes for upper bound

;Exercise 2.10

(define (spans-zero? a)
  (and (negative? (lower-bound a)) (positive? (upper-bound a))))

(define div-interval2
  (lambda (x y)
    (if (spans-zero? y)
        (error "The denominator should not span 0")
        (mul-interval x (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))))))



      