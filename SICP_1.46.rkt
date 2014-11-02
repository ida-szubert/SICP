#lang racket

;square root
(define (sqrt x)
   (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
   (if (good-enough? guess x)
       guess
       (sqrt-iter (improve guess x)
                  x)))

(define (improve guess x)
   (average guess (/ x guess)))

(define (average x y)
   (/ (+ x y) 2))

(define (good-enough? guess x)
 (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))


; fixed-point
(define (fixed-point f first-guess)
   (define (close-enough? v1 v2)
     (< (abs (- v1 v2)) 0.00001))
   (define (try guess)
     (let ((next (f guess)))
       (if (close-enough? guess next)
           next
           (try next))))
   (try first-guess))


;iterative-improve
(define iter-improve
  (lambda (good-enough? improve-guess)
    (define (help-function guess)
       (if (good-enough? guess)
           guess
           (help-function (improve-guess guess))))
    help-function))

;square root
(define new-sqrt
  (lambda (x)
    ((iter-improve good-enough? improve) 1.0)))

;fixed point
(define new-fixed-point
  (lambda (f first-guess)
    ((iter-improve (lambda (x y) (< (abs (- x y)) 0.00001)) f) first-guess)))

 