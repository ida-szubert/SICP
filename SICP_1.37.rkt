#lang racket
;approximating infinite continued fraction
;you can stop after a given number of terms (k-term finite continued fraction)
;n and d are procedures, both of witch take one argument (the term index i) and return the Ni and Di respectively
;Ni and Di are the N and D terms in the i-th iteration of the pattern


(define cont-frac
  (lambda (n d k)
    (define help
      (lambda (counter)
        (if (> counter k)
            1
            (/ (n counter) (+ (d counter) (help (+ counter 1)))))))
    (help 0)))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
(/ 1.0 (+ 1.0 (/ 1.0 (+ 1.0 (/ 1.0 (+ 1.0 (/ 1.0 (+ 1.0 1))))))))
;k=3 0.625
;k=4 0.61538
;k=5 0.61904
;k=6 0.61764
;k=7 0.61818
;k=8 0.61797
;k=9 0.61805
;k=10 0.61802


;iterative version
         
(define cont-frac-iter
  (lambda (n d k)
    (define help
      (lambda (counter result)
        (if (zero? counter)
            result
            (help (- counter 1) (/ (n counter) (+ (d counter) result))))))
    (help (- k 1) (/ (n k) (d k)))))

(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10)
;(help 2 1.0)
;(help 1 (/ 1.0 (+ 1.0 1.0))) --> (help 1 0.5)
;(help 0 (/ 1.0 (+ 1.0 0.5)))
(/ 1.0 1.5)

;The results of cont-frac and cont-frac-iter are different when k is relatively small

;k=3 0.666 0.625
;k=4 0.600 0.61538
;k=5 0.625 0.61904
;k=6 0.615 0.61764
;k=7 0.619 0.61818
;k=8 0.617 0.61797
;k=9 0.618 0.61805
;k=10 0.617 0.61802

;but they converge for larger values of k
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 100)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)
;0.6180339887498948
