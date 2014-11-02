#lang planet neil/sicp

(define f
  (lambda ( x y)
    (define f-helper
      (lambda (a b)
        (+ (* x (square a))
           (* y b)
           (* a b))))
    (f-helper (+ 1 (* x y)) (- 1 y))))

;were the f-helper an anonymous procedure, it would look like this:
(define f
  (lambda (x y)
    ((lambda (a b)
       (+ (* x (square a))
          (* y b)
          (* a b)))
     (+1 (* x y)) (- 1 y))))

;using let, it looks like this:
(define f
  (lambda (x y)
    (let ((a (+ 1 (* x y)))
          (b (- 1 y)))
      (+ (* x (square a))
         (* y b)
         (* a b)))))

;let expression is syntactic sugar; the interpreter translates it to the lambda expression, just like the one in the second version of f
;the scope of variables specified by let is the body of let
;so the variables are locally bound
;if a value ascribed to a variable is an expression, that expression will be evaluated outside of let
;we could use definitions to sepcify a and b
(define (f x y)
  (define a (+1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a)) (* y b) (* a b)))
;but it's better to use let to specify variables
;and leave internal definitions for when we need to define an internal procedure


