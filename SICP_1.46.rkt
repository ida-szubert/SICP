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
    (define (help guess)
       (if (good-enough? guess)
           guess
           (help (improve-guess guess))))
    help))

;square root
(define new-sqrt
  (lambda (x)
    (define good-enough?
      (lambda (a) (< (abs (- (square a) x)) 0.001)))
    (define improve
      (lambda (a) (average a (/ x a)))) 
    ((iter-improve good-enough? improve) 1.0)))

(new-sqrt 81)
(new-sqrt 2)

;fixed point
(define new-fixed-point
  (lambda (f guess)
    (define good-enough?
      (lambda (a) (< (abs (- (f a) a)) 0.00001)))
    ((iter-improve good-enough? f) (f guess))))

(new-fixed-point cos 1.0)
(fixed-point cos 1.0)

 