#lang racket

;Exercise 4.19
(define try
  (let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10)))

try

;a of let hes scope over all the rest of the expressions
;but define creates a new environment
;in which a is bound to 5
;and b sould be 5 + x
;and f should return 10 + x
;and the initial a seems to be of no consequence
;<-- that's whta one can expect if the internal definitions of a and b are indeed simultaneous

;However, according to rules implemented in 4.16 (and in Racket), the value of a will be *unassigned* when we try to copute the value of b
;and an error will result

;how to implement internal definitions so that they behave in the desired way
;i.e. how to allow for mutual reference between internal definitins?
;couldn't we just not evaluate the expressions when they are being bound to variables, but only when the variables are called?
;i.e the environmnt would store bindings of variables and expressions, instead of bindings of variables and values

;Argh, I don't know