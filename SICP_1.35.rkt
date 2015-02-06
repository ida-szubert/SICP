#lang racket
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

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;1.6180327868852458
;phi is the fixed point of this function
;that's because phi is a number that fulfills the following equation
;x^2 = x + 1
;x = (x + 1) / x
;x = (x / x) + (1 / x)
;x = 1 + (1 / x) 