#lang racket

;Exercise 4.25

;assuming applicative-order evaluation
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

;the body of the factorial procedure is
(if (= n 1)
    1
    (* n (factorial (- n 1))))
;which looks fine.

;But before we ever get to the body, we need to evaluate the arguments of unless
;(= 5 1) evaluates to #f
;(* 5 (factorial 4)) needs (factorial 4) evaluated ->
   ;(= 4 1) #f
   ;(* 4 (factorial 3))
      ;(= 3 1) #f
      ;(* 3 (factorial 2))
         ;(= 2 1) #f
         ;(* 2 (factorial 1))
            ;(= 1 1) #t
            ;(* 1 (factorial 0))
               ;...

;the evaluation never stops because there is no stopping condition

;If we were, however, operating under normal-order evaluation, thing would go ok
;we would enter the if expression, evaluate the condition, which poses no problems, and when we get to calculating (factorial 1)