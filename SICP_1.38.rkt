#lang planet neil/sicp

(define cont-frac
  (lambda (n d k)
    (define help
      (lambda (counter)
        (if (> counter k)
            1
            (/ (n counter) (+ (d counter) (help (+ counter 1)))))))
    (help 0)))

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
  
