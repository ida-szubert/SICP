#lang racket

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons x y)) ;what does it yield?

(lambda (m)
  (m x y)) ;applied to
(lambda (p q)
  p)
;so
((lambda (p q) p) ;applied to
x y)
;so
x

(define (cdr z)
  (z (lambda (p q) q)))
