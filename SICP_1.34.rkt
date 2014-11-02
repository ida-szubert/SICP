#lang planet neil/sicp

;a procedure which takes one argument, which is also a procedure of one argument, and applies it to 2
(define f
  (lambda (g)
    (g 2)))

(define (square x) (* x x))

(f square)
(f (lambda (z) (* z (+ z 1))))
         
;what happens when we ask about the value of (f f)
(f f)
;"application: not a procedure; expected a procedure that can be applied to arguments given: 2"
;it cannot be evaluated because when we try to apply f to 2, we get the invalid expression (2 2).
;there is no operator in (2 2), and so this list has no value