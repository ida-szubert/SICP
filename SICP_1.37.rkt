#lang racket
;approximating infinite continued fraction
;you can stop after a given number of terms (k-term finite continued fraction)
;n and d are procedures, both of witch take one argument (the term index i) and return the Ni and Di respectively
;Ni and Di are the N and D terms in the i-th iteration of the pattern

(define cont-frac-help
  (lambda (n d k counter)
    (if (> counter k)
        1
        (/ (n counter) (+ (d counter) (cont-frac-help n d k (+ counter 1)))))))

(define cont-frac
  (lambda (n d k)
    (cont-frac-help n d k 0)))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
;k=3 0.625
;k=4 0.61538
;k=5 0.61904
;k=6 0.61764
;k=7 0.61818
;k=8 0.61797
;k=9 0.61805
;k=10 0.61802


;iterative version
(define cont-frac-iter-help
  (lambda (n d k counter result)
    (if (zero? counter)
        result
        (cont-frac-iter-help n d k (- counter 1) (/ (n counter) (+ (d counter) result))))))
         
(define cont-frac-iter
  (lambda (n d k)
    (cont-frac-iter-help n d k (- k 1) (/ (n k) (d k)))))


(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10)
;funny, the resultsof cont-frac and cont-frac-iter are slightly different