#lang racket

(define cont-frac
  (lambda (n d k)
    (define help
      (lambda (counter)
        (if (> counter k)
            1
            (/ (n counter) (+ (d counter) (help (+ counter 1)))))))
    (help 0)))

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
;0.9999999999999999
;1.0
(tan (/ pi 5))
(tan-cf (/ pi 5) 10)
;0.7265425280053608
;0.7265425280053608
(tan (/ pi 8))
(tan-cf (/ pi 8) 10)
;0.41421356237309503
;0.41421356237309503
       
