#lang racket

;Exercise 4.15
;Is it impossible to write a predicte halts? that would determine whether p halts on a, for any procedure p and object a?

;Let's say we do have the procedure halts?
;We can define a procedure that never stops, e.g. one that does nothing else but call itself:
(define (run-forever) (run-forever))

;let's define a procedure try
(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))
;it takes a procedure as an argument
;asks whether the procedure halts when applied to itself
;if it does, try calls run-forever
;if it doesn't, try returns 'halted
;so, if (p p) halts, (try p) doesn't halt
;if (p p) doesn't halt, (try p) halts

;what happens when we evaluate (try try)?
;if (try try) halts, (try try) doesn't halt
;if (try try) doesn't halt, (try try) halts

;it is impossible to write a procedure halts? that would work when applied to try and try

