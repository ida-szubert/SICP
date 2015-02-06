#lang racket

(define (another-cons x y)
  (lambda (m) (m x y)))

(define (another-car z)
  (z (lambda (p q) p)))

;(another-car (another-cons x y))
;what does it yield?

;((lambda (m) (m x y)) (lambda (p q) p))
;((lambda (p q) p) x y)
;x

(define (another-cdr z)
  (z (lambda (p q) q)))

(define a (another-cons 3 4))
(another-car a)
(another-cdr a)