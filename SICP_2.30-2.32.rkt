#lang racket

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
(scale-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

;Exercise 2.30
(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree)))
       tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;Exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(tree-map square (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (inc x) (+ x 1))
(tree-map inc (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;Exercise 2.32
(define (subset s)
  (if (null? s)
      (list '())
      (let ((rest (subset (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subset (list 1 2 3))
