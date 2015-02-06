#lang racket

(define (make-interval a b)
  (cons a b))
(define (lower-bound x)
  (car x))
(define (upper-bound x)
  (cdr x))


(define mul-original
  (lambda (x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
         (p2 (* (lower-bound x) (upper-bound y)))
         (p3 (* (upper-bound x) (lower-bound y)))
         (p4 (* (upper-bound x) (upper-bound y))))
      (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))))

;possible combinations of signs of the boundaries of two intervals
;(+ -) is not taken into account, bacause lower boundary cannot be more than upper boundary
;(+ +) (+ +)
;(+ +) (- +)
;(+ +) (- -)

;(- +) (- +)
;(- +) (- -)
;(- +) (+ +)

;(- -) (- -)
;(- -) (- +)
;(- -) (+ +)

;for most of these combinations we know imediately what to multiply to get the result of mul-interval
;in the case of (- +) (- +) we need all four multiplications, because the product of the lower bounds might be higher then the product of the upper bounds
;when one of the intervals spans zero, we know that to get the lower bound we should multiply lower bound of x and upper bound of y (or vice versa)
;but intead of adding two extra conditions, we can just treat it the same as multiplication of two zero-spanning intervals
;there will be unneccessary calculations made
;but the function will be simpler to read


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

(define x (make-interval 6.12 7.48))
(define y (make-interval 4.47 4.94))
(define z (make-interval -2.25 1.45))

(lower-bound (mul-interval x y))
(upper-bound (mul-interval x y))
(lower-bound (mul-interval y z))
(upper-bound (mul-interval y z))
(lower-bound (mul-interval z z))
(upper-bound (mul-interval z z))

(lower-bound (mul-original x y))
(upper-bound (mul-original x y))
(lower-bound (mul-original y z))
(upper-bound (mul-original y z))
(lower-bound (mul-original z z))
(upper-bound (mul-original z z))
         
         
         
          
      