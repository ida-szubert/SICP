#lang planet neil/sicp

(define cont-frac-help
  (lambda (n d k counter)
    (if (> counter k)
        1
        (/ (n counter) (+ (d counter) (cont-frac-help n d k (+ counter 1)))))))

(define cont-frac
  (lambda (n d k)
    (cont-frac-help n d k 1)))

(define euler
  (lambda (k)
    (+ 2 (cont-frac (lambda (i) 1.0) euler-d k))))

(define euler-d
  (lambda (k)
    (if (not (= (remainder (+ k 1) 3) 0))
        1
        (* 2 (/ (+ k 1) 3)))))

(euler 10)
;2.718282368249837
  
