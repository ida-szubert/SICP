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
(+ +) (+ +)
(+ +) (- +)
(+ +) (- -)

(- +) (- +)
(- +) (- -)
(- +) (+ +)

(- -) (- -)
(- -) (- +)
(- -) (+ +)

;for most of these combinations we know imediately what to multiply to get the result of mul-interval
;in the case of (- +) (- +) we need all four multiplications, because the product of the lower bounds might be higher
;then the prodcut of the upper bounds
;instead of testing the signs, we might use spans-zero? from the previous exercise

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


         
         
         
          
      