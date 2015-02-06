#lang racket

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

;Exercise 2.24
(list 1 (list 2 (list 3 4)))
;(1 2 3 4)
;actually (mcons 1 (mcons (mcons 2 (mcons (mcons 3 (mcons 4 '())) '())) '()))
;and it doesn't evaluate futher

;Exercise 2.25
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(cadr (caddr '(1 3 (5 7) 9)))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))


;Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(cons x y)
;((1 2 3) 4 5 6)
;we are consing x onto y, so x becomes the first element of the new list
(list x y)
;((1 2 3) (4 5 6))
;we are forming a list of two elements
(append x y)
;(1 2 3 4 5 6)
;we are joining two lists into one

;Exercise 2.27
(define (reverse l)
  (cond ((null? l) l)
        ((pair? (car l)) (append (reverse (cdr l)) (list (reverse (car l)))))
        (else (append (reverse (cdr l)) (cons (car l) '())))))
        
(define z (list 1 3 (list 5 6 7) 3 4))
(define v (list (list 2 3) 4 5 (list 5 (list 6 7))))
(reverse z)
(reverse v)


;Exercise 2.28
(define (fringe l)
  (cond ((null? l) l)
        ((pair? (car l)) (append (fringe (car l)) (fringe (cdr l))))
        (else (cons (car l) (fringe (cdr l))))))

(fringe (list (list 1 2) (list 3 4)))
(fringe z)
(fringe '())


;Exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;a.
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))

;b.
(define (weight branch)
  (if (pair? (structure branch))
      (total-weight (structure branch))
      (structure branch)))

(define (total-weight mobile)
  (+ (weight (left-branch mobile)) (weight (right-branch mobile))))

;c.
(define (length branch)
  (car branch))
(define (structure branch)
  (car (cdr branch)))

(define (balanced? mobile)
  (define (torque branch) (* (weight branch) (length branch)))
  (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
       (balanced-branch? (left-branch mobile))
       (balanced-branch? (right-branch mobile))))

(define (balanced-branch? branch)
  (if (pair? (structure branch))
      (balanced? (structure branch))
      #t))

(define a (make-mobile (make-branch 3 4) (make-branch 6 2)))
(define b (make-mobile (make-branch 3 4) (make-branch 4 5)))

(total-weight a)
(total-weight b)
(balanced? a)
(balanced? b)

        

 