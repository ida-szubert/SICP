#lang racket
;finding foots of equations:

;half-interval method
;given a and b such that f(a) < 0 < f(b)
;there has to be at least one value c between a and b such that f(c) = 0
;the's stay that x = (average a b)
;if f(x) > 0, then let's look for the root between a and x
;if f(x) < 0, let's look between x and b

(define search
  (lambda (f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
      (if (close-enough? neg-point pos-point)
          midpoint
          (let ((test-value (f midpoint)))
            (cond ((positive? test-value) (search f neg-point midpoint))
                  ((negative? test-value) (search f midpoint pos-point))
                  (else midpoint)))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define (average x y) (/ (+ x y) 2))

;using 'search is not convenient; you need to know the values of a and b such that f(a) is negative and f(b) is positive

(define half-interval-method
  (lambda (f a b)
    (let ((a-value (f a))
          (b-value (f b)))
      (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
            ((and (positive? a-value) (negative? b-value)) (search f b a))
            (else (error "Values are not of opposite sign" a b))))))

;let's approximate pi as the root between 2 and 4 of sin x = 0
(half-interval-method sin 2.0 4.0)
; 3.14111328125

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)



;finding fixed points of functions
;such that f(x) = x
;for some functions we can find it applying f to an initial guess and then reapplying f untill the result doesn't change much
(define fixed-point
  (lambda (f first-guess)
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) 0.00001))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(fixed-point cos 1.0)
;0.7390822985224023

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
;1.2587315962971173

;let's go back to Newton's procedure for computing square root
(define (square x) (* x x))

(define (sqrt x) (sqrt-iter 1.0 x))
  
(define sqrt-iter
  (lambda (guess x)
    (define (good-enough? a b) (< (abs (- (square a) b)) 0.001))
    (define (improve guess x) (average guess (/ x guess)))
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))
;finding sqrt of x means finding y such that y^2 = x
;in other words, we want y such that y = x/y
;so we're looking for a fixed point of the function y -> x/y

;(define (sqrt-fp x)
 ; (fixed-point (lambda (y) (/ x y)) 1.0))

;this doesn't work
;initial guess is y1
;second guess is x/y1
;third guess is x/(x/y1) = y1

;we can do something to prevent the guesses from changing so much
;the answer is somewhere between the guess and the improved guess
;we can average y and (x/y) and take this as the next guess (instead of (x/y)) 
;this is called average dumping and often aids the convergence of fixed-point searches

(define (sqrt-fp x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))


      
      
