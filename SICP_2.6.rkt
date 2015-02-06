#lang racket

(define (inc x)
  (+ x 1))

(define zero 
  (lambda (f)
    (lambda (x) x)))
;zero is a function which takes one argument and returns an identity function
;zero is a procedure which applies it's input procedure 0 times
((zero inc) 0)
((zero inc) 5)


(define add-1
  (lambda (n)
    (lambda (f) (lambda (x) (f ((n f) x))))))      
;add-1 is a function which takes an argument n and returns a function
;this function takes a function f as its argument
;and returns yet another function
;which takes one argument, applies (n f) to this argument
;and applies f to the result
;so: we have two functions, f and n
;one of which wants a funcion as its argument, and the other which wants a value
;zero is a function which wants a function as its argument
;it'd suitable in the n position

;evaluate (add-1 zero)
;it should be a procedure which applies the input procedure 1 time
;(lambda (f) (lambda (x) (f ((zero f) x))))
;(lambda (f) (lambda (x) (f (lambda (x) x) x)))
;(lambda (f) (lambda (x) (f x)))
;so, given f and x, we apply f to x once

;one
;it should be a  procedure which applies it's input procedure once
(define one
  (lambda (f)
    (lambda (x) (f x))))

;two
;should apply f twice
(define two
  (lambda (f)
    (lambda (x) (f (f x)))))
  
((one inc) 5)
((two inc) 5)

(define add
  (lambda (m n)
    (lambda (f) (lambda (x) ((m f) ((n f) x))))))
;to add 1 we wrap the number n into one additional call to f
;to add m we wrap n into m additional calls to f

(((add one two) inc) 0)
    