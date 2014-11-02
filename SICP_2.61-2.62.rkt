#lang racket

;sets as ordered lists
;we need a comparison procedure to be able to order lists
;let's stick to numbers and use > and <

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f) ;because (car ste) is the smallest element of the set, and if x is even smaller, it won't be in found in (cdr set)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set (cdr set1) set2))
              ((> x1 x2) (intersection-set set1 (cdr set2)))))))

;Exercise 2.61
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cond ((or (null? set) (< x (car set))) (cons x set))
            ((> x (car set)) (cons (car set) (adjoin-set x (cdr set)))))))

(adjoin-set 5 '(1 3 8))

;Exercise 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                      ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))


         