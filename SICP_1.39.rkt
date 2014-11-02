#lang racket

(define cont-frac-help
  (lambda (n d k counter)
    (if (> counter k)
        1
        (/ (n counter) (+ (d counter) (cont-frac-help n d k (+ counter 1)))))))

(define cont-frac
  (lambda (n d k)
    (cont-frac-help n d k 1)))

(define tan-cf
  (lambda (x k)
    (define (n k)
      (if (= k 1)
          x
          (- (* x x))))
    (define (d k)
      (- (* 2 k) 1))
    (cont-frac n d k)))

(tan (/ pi 4))
(tan-cf (/ pi 4) 10)
(tan (/ pi 5))
(tan-cf (/ pi 5) 10)
(tan (/ pi 8))
(tan-cf (/ pi 8) 10)

;0.9999999999999999
;1.0
;0.7265425280053608
;0.7265425280053608
;0.41421356237309503
;0.41421356237309503
       
