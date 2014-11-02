#lang planet neil/sicp

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

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define newton-transform
  (lambda (g)
    (lambda (x) (- x (/ (g x) ((deriv g) x))))))

(define newtons-method
  (lambda (g guess)
    (fixed-point (newton-transform g) guess)))

(define cubic
  (lambda (a b c)
    (lambda (x)
      (+ (* x x x) (* a x x) (* b x) c))))
    
(newtons-method (cubic 3 -2.4 6) 1)
;we need to feed newtons-method with the function we want to find the zero of
;this function gets transformed
;and we look for a fixed point of the transformed function
;this fixed point is equal to the zero point of the original function