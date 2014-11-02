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
    (if (and (spans-zero? x) (spans-zero? y))
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
(define aa (div-interval a  a))
(percent aa); it's double (percent a)

(define cc (div-interval c c))
(percent cc); it's double (percent c)

;A/B
(define ab (div-interval a b))
(percent ab); it's (percent a) + (percent b)

(define cd (div-interval c d))
(percent cd); it's (percent c) + (percent b)

(par1 a b)
(par2 a b)
(par1 c d)
(par2 c d)
(par1 e f)
(par2 e f)

(percent (make-interval 1 1))

;results of par2 seem to have better precision
;the lower the precision o the input values, the greater the difference between precision of the result of par1 and par2

;everytime we multiply or divide intervals, we decrease precision

;in par1 we multiply r1 and r2, and then we divide the result by the sum of r1 and r2
;there are 2 operations that significantly reduce precision
;in par2 there are two divisions 1/r, but these do nothing to the precision, because (percent one) = 0
;then we add (1/r1) to (1/r2) and divide 1 by that sum- again, no changes to precision

;for both par1 and par2 the result has lower precision than the input values because of addition
;but only for par1 the precision gets decreased futher by multiplication and division
