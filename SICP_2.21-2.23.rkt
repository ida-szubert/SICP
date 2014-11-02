#lang planet neil/sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))
(map (lambda (x) (- (* x x) 1)) (list 1 2 3 4))


;Exercise 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map square items))

(define (square x) (* x x))


;Exercise 2.22
(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons (square (car things)) answer))))
  (iter items nil))

;this definition produces a list of squares, but the order is reversed relative to the originial list
;that's because at aeach step we cons the square of (car l) onto the answer list
;i.e. we cons square of the fist element onto (), then we cons square of the second element onto that, etc.

(define (square-list-iter2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons answer (square (car things))))))
  (iter items nil))

(square-list-iter (list 1 2 3 4))
(square-list-iter2 (list 1 2 3 4))
;the second take doesn't work because you cannot cons '() onto a number and get a list
;it has to be the other way round


;Exercise 2.23
(define (for-each proc list)
  (cond ((not (null? list))
        (proc (car list))
        (for-each proc (cdr list)))))

(define (print x)
  (newline)
  (display x))

(for-each print (list 2 4 6 8 9))