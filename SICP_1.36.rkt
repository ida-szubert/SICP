#lang planet neil/sicp
(define fixed-point
  (lambda (f first-guess)
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) 0.00001))
    (define (try guess)
      (let ((next (average guess (f guess))))
        (display guess)
        (newline)
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (average x y) (/ (+ x y) 2))
             

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
;without average dumping (let ((next (f guess))) : 34 steps
;with average dumping (let ((next (average guess (f guess)))) : 9 steps
