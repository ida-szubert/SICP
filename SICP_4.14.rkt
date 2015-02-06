#lang racket

;Exercise 4.14

;defining map as follows in the global environment works just fine
;(define (map proc l) (if (null? l) '() (cons (proc (car l)) (map proc (cdr l)))))

;what we're doing is binding the variable 'map to the expression
;(lambda (proc l) (if (null? l) '() (cons (proc (car l)) (map proc (cdr l)))))
;ie. map is build as a compound procedure
;when map is called, this expression gets evaluated by the metacircular evaluator in the global environment
;and all null?, cons, car, and cdr-s are called as primitive procedures 

;but usually we don't need to define map, as it comes preinstalled
;we could list this map as one of the primitive operations, instead of installing it in the environment as an expression that needs to be evaluated every time map is used 
;we encounter a problem with applying the primitive map
;it seems that we've created map which uses cons, car, and cdr procedures that are not understood by the evaluator
;these are the underlying scheme procedures, not the cons, car, and cdr we;ve installed as primitive

;for some reason the function passed to (primitive-) map is not recognized as a procedure


