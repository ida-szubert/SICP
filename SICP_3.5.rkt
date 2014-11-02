#lang racket

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
  iter trials 0)

(define (estimate-integral p x1 x2 y1 y2 trials)
  (let ((rectangle-area (* (abs (- x2 x1)) (abs (- y2 y1)))))
  (define (predicate-test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (p x y)))
  (* rectangle-area (monte-carlo trials predicate-test))))

(define (predicate x y)
    (<= (+ (square x) (square y)) 1)) ;for unit circle centred on (0,0)

(define (square x) (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(estimate-integral predicate -2 2 -2 2 1000000)

;doesn't work, and I can't figure out why