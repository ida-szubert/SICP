#lang racket

;Representing sets

;we can define set by specifying the operations used on sets
;union, intersection, adjunction, element-of-set?

;set can be represented as unordered list, each element of which occurs only once

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection (cdr set1) set2)))
        (else (intersection (cdr set1) set2))))

;what about considerations of efficiency?
;e.g the speed of intersection and adjoin-set depends on the speed of element-of-set?
;in order to check if x is a member of a set1, the program might need to go through the whole set
;the order of growth for element-of-set? is O(n)
;it's the same for adjoin-set
;for intersection the number of steps required grows as the product of the sizes of the sets, so O(n^2) for two sets of size n

;Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define set1 '(1 3 6 9 12 15 18))
(define set2 '(1 2 4 6 8 10 12))

(element-of-set? 5 set1)
(intersection set1 set2)
(element-of-set? 1 (intersection set1 set2))
(union-set set1 set2)
(union-set set2 set1)

;Exercise 2.60
;what if we allow for duplicates?
;the set {1, 2, 3} could be represented as (2 3 2 1 3 2 2)

(define (dup-element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? (car set) x) #t)
        (else (dup-element-of-set? x (cdr set)))))
;this procedure does not need to change to accomodate the new representation

(define (dup-adjoin-set x set)
  (cons x set))

(define (dup-union-set set1 set2)
  (append set1 set2))

(define (dup-intersection set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2) (cons (car set1) (dup-intersection (cdr set1) set2)))
        (else (dup-intersection (cdr set1) set2))))
;intersection doesn't change as well
;if set1 contains duplicate elements intersection will do unnecessary job- having once checked that x is an element of set2
;it will have to check it again every time x appears in set1
;also, intersection will output different results depending on the order of arguments
;it will not output different sets, but different representations
;if set one contains 3 x-s, and set2 sontains one x, the output list will contain 3 x-s
;if we switch the order, the output list will contain one x
