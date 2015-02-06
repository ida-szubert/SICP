#lang planet neil/sicp
(#%require (only math/base random-natural))

;Exercise 3.5
;Monte Carlo integration
;computing the area of a region of space described by a predicate P(x,y) (i.e. p is #t of (x,y) in the region)
;circle r=3, x=5 y=7. The area is described by the predicate (x-5)^2 + (y-7)^2 =< 3^2
;to estimate the area we first need to specify a region which contains this area
;pick at random points (x,y) within the chosen region
;as test is p(x,y) is true
;the fraction of the points for which the predicate is true is an estimate of te fraction of the rectangle which forms the area of te circle

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (let ((rectangle-area (* (abs (- x2 x1)) (abs (- y2 y1)))))
  (define (predicate-test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (p x y)))
  (* rectangle-area (monte-carlo trials predicate-test) 1.0))) ;the 1.0 is here so that I get decimal expansion instead of a fraction

;x^2 + y^2 =< 1^2
(define (square x) (* x x))

(define (predicate x y)
    (<= (+ (square x) (square y)) 1))

(define (random-in-range-1 low high)
  (let ((range (- high low)))
    (+ low (random range))))

;(/ (estimate-integral predicate -10 10 -10 10 1000000) 100.0)
;random is not really an appropriate procedure, since it expects integers, and returns integers
;so if I'm thinking about a unit square circle, and want random numbers in the range -1 to 1, random is going to give me 0 and -1 only
;ok, using a circle of radius 10 is better
;but let's try 1 anyway

;using unit square, with this less-then-perfect way of generating random numbers, the result is mediocre
;3.001044
;estimating pi using a circle of radius 10 is way better
;3.148932
;the estimate should be equally precise for a unit-square circle had I a better random-in-range
;actually:
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (/ (random (* 100 range)) 100.0))))
;not perfect, but it works

(estimate-integral predicate -1 1 -1 1 1000000)
;3.14512