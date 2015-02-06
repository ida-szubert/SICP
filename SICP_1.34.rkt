#lang planet neil/sicp

(define f
  (lambda (g)
    (g 2)))
;a procedure which takes one argument, which is also a procedure of one argument, and applies it to 2

(define (square x) (* x x))

(f square)
;(square 2)
(f (lambda (z) (* z (+ z 1))))
;(* 2 (+ 2 1))
         
;what happens when we ask about the value of (f f)
(f f)
;"application: not a procedure; expected a procedure that can be applied to arguments given: 2"
;it cannot be evaluated. We cannot apply f to 2, since f takes as its argument a function, not a value.
