#lang racket
;Exercise 2.37
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs)
            (accumulate-n op init (map cdr seqs))))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(define (transpose m)
  (accumulate-n list '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;I don't really get why n has to be transposed prior to matrix-*-vector

;Exercise 2.38
;accumulate is the same as fold-right
;it combines the first element of the sequence with the result of combining all of the elements on the right

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3)) ; 1 1/2
(fold-left / 1 (list 1 2 3)) ; 1/6
(fold-right list '() (list 1 2 3)) ; '(1 (2 (3 ())))
(fold-left list '() (list 1 2 3)) ; '(((() 1) 2) 3)
;if fold-left and fold-right are to produce the same result, op must be a comutative operation

;Exercise 2.39
(define (reverseR sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

;(define (accumulate op initial sequence)
;  (if (null? sequence)
;      initial
;      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (reverseL sequence)
  (fold-left (lambda (x y) (append (list y) x)) '() sequence))

(reverseR (list 1 2 3))
(reverseL (list 1 2 3))
  
  