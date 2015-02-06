#lang racket

;Exercise 2.8
;In the substitution model we start with evaluating the subexpressions
;the order was never specified
;with value assignment it can make a difference
;Define a simple procedure f such that evaluating
;(+ (f 0) (f 1))
;will return 0 if the arguments to + are evaluated from le to right
;and  1 if the arguments are evaluated from right to le.

  
(define (foo n)
  (let ((state 1))
    (begin (set! state (* state n))
           state)))

(define (g y) 
   (define (f x) 
     (let ((z y)) 
       (set! y x) 
       z)) 
   f) 
(define f (g 0)) 


(+ (f 0) (f 1))
(+ (f 1) (f 0))