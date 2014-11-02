#lang planet neil/sicp

(define double
  (lambda (f)
    (lambda (x) (f (f x)))))

(define inc
  (lambda (x)
    (+ x 1)))
(define (square x) (* x x))

((double inc) 0)
((double square) 3)

(((double (double double)) inc) 5)
(+ 16 5)
;((double (double double)) inc)
;(double (double double))
;(double double)
;apply double to double: (lambda (x) (double (double x)))
;apply double to that: (lambda (x) (double (double (double (double x)))))
;apply this to inc: (double (double (double (double inc))))
;(double inc) amounts to 
;(inc (inc x)) - this adds 2
;(double (inc (inc x))) - this adds 4
;(double (double (inc (inc x)))) - this adds 8
;(double (double (double (inc (inc x))))) - this adds 16