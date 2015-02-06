#lang racket
(define (make-interval a b)
  (cons a b))

(define (make-center-percent c p)
  (make-interval  (- c (* (/ p 100) c)) (+ c (* (/ p 100) c))))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (percent i)
  (* (/ (- (upper-bound i) (center i)) (center i)) 100))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

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

(define add-interval
  (lambda (x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y)))))

(define div-interval
  (lambda (x y)
    (mul-interval
     x
     (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))))


;Exercise 2.15
;there are two methods for calculating parallel resistance of two resistors
; (r1*r2) / (r1+r2)
(define par1
  (lambda (r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2))))

; 1 / ((1/r1) + (1/r2))
(define par2
  (lambda (r1 r2)
    (let ((one (make-interval 1 1)))
      (div-interval one
                    (add-interval (div-interval one r1) (div-interval one r2))))))


(define a (make-center-percent 2 1.5))
(define b (make-center-percent 3.5 1))
(define c (make-center-percent 7.14 0.15))
(define d (make-center-percent 5.67 0.10))
(define e (make-center-percent 4.7 5))
(define f (make-center-percent 6.8 10))

;A/A
(percent (div-interval a  a))
; it's double (percent a)

(percent (div-interval c c))
; it's double (percent c)

;A/B
(percent (div-interval a b))
; it's (percent a) + (percent b)

(percent (div-interval c d))
; it's (percent c) + (percent b)

(par1 a b)
(par2 a b)
(par1 c d)
(par2 c d)
(par1 e f)
(par2 e f)

(percent (make-interval 1 1))

;results of par2 seem to have better precision
;the lower the precision of the input values, the greater the difference between precision of the result of par1 and par2
;why is it the case?

;everytime we multiply or divide intervals, we decrease precision

;in par1 we multiply r1 and r2, and then we divide the result by the sum of r1 and r2
;there are 2 operations that significantly reduce precision
;in par2 there are two divisions 1/r, but these are harmless, because (percent one) = 0, and addintg 0 to (percent r) changes nothing
;then we add (1/r1) to (1/r2) and divide 1 by that sum- again, no changes to precision

;for both par1 and par2 the result has lower precision than the input values because of addition
;but only for par1 the precision gets decreased futher by multiplication and division



;Exercise 2.16
;"This problem is very difficult"