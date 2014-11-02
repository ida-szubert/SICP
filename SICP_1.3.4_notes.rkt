#lang racket
; procedures can be return values

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
    (lambda (x) (average x (f x))))
  
((average-damp square) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

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

;Newton's method
;if g(x) is a differentiable function, then a solution of g(x) = 0 is a fixed point of the function f(x), such that
;f(x) = x - (g(x) / Dg(x))
;where Dg(x) is the derivative of g evaluated at x
;Newton's method converges of the fixed point much more rapidly then the half-interval method

;so what is derivative?
;like average-damping, derivative takes a function and outputs a function
;g is a function, and dx is a small number
;derivative of g is the function whose value at x is given by
;Dg(x) = (g(x + dx) - g(x)) / dx
;so we take a value of g for x
;we take the difference between g(x) and g(y), where y is slightly larger than y
;and divide the result by this slight difference between y and x

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

((deriv cube) 5)
;75.00014999664018


;let's write the Newton's method as a fixed-point process
(define newton-transform
  (lambda (g)
    (lambda (x) (- x (/ (g x) ((deriv g) x))))))

(define newtons-method
  (lambda (g guess)
    (fixed-point (newton-transform g) guess)))


(define (newton-sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))



;so we've written two fixed-point methods for sqrt
;both begin with a function, transform it somehow, and find the fixed point
(define fixed-point-of-transform
  (lambda (g transform guess)
    (fixed-point (transform g) guess)))

(define new-sqrt
  (lambda (x)
    (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0)))

(define new-newton-sqrt
  (lambda (x)
    (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0)))