#lang racket

;(define (even-fibs n)
;  (define (next k)
;    (if (> k n)
;        '()
;        (let ((f (fib k)))
;          (if (even? f)
;              (cons f (next (+ k 1)))
;              (next (+ k 1))))))
;  (next 0))

;enumerate:  program enumerates integers from 0 to n
;map: computes the Fibonacci number for each
;filter: filters the odd numbers out
;accumulate: accumulates the results using cons
;these stages are not obvious from the structure of the program

;how to show signal flow structure in the structure of a program?
;using abstract functions might be a good idea

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;this is the fringe function from one of the exercises
(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))

;now we can write an information-flow-compliant definition for the summ of odd squares of leaves of a tree
(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (square x) (* x x))

;what about even Fibonacci numbers?
;(define (fibs-even n)
;  (accumulate
;   cons
;   '()
;   (filter even? (map fib (enumerate-interval 0 n)))))

;why should programs be written that way?
;modularity is good
;modules are portable
;you can use ready-made modules
;the design is transparent
;when you're solving a particular higher-level problem you really don't need to make it explicit how filtering happens
;it's better to simply use a filter function, defined elsewere, and focus of solving the main problem

;Exercise 2.33
;(define (accumulate op initial sequence)
;  (if (null? sequence)
;      initial
;      (op (car sequence) (accumulate op initial (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;Exercise 2.35
(define (count-leaves x)
   (cond ((null? x) 0)
         ((not (pair? x)) 1)
         (else (+ (count-leaves (car x))
                  (count-leaves (cdr x))))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

;starting with 0, we are adding the elements of the sequence
;the sequence is created by flattening the tree into a list of its leaves (enumerate-tree t)
;and then changing every element of that list into a 1

;Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs)
            (accumulate-n op init (map cdr seqs))))))





             
  
  
                     