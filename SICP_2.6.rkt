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
      
;add-1 is a function which takes one argument and returns a function
                 ;this function takes one function as its argument

;evaluate (add-1 zero)
;it should be a procedure which applies the input procedure 1 time
;(lambda (f) (lambda (x) (f ((zero f) x))))
;(lambda (f) (lambda (x) (f (lambda (x) x) x)))
;(lambda (f) (lambda (x) (f x)))
;so, given af and x, we apply f to x once

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
    