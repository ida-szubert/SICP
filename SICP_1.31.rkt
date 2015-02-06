#lang planet neil/sicp

(define (double x) (* 2 x))
(define (inc x) (+ x 1))
(define (identity x) x)

;write a high-level procedure analogous to sumation, but outputing product rather than sum
(define product
  (lambda (term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b)))))

(product double 1 inc 3)
(product double -1 inc 3)
(product double 0 inc 0)

;define factorial in terms of product
(define factorial
  (lambda (n)
    (product identity 1 inc n)))

(factorial 6)

;compute an approximation of pi using formula from the book
(define (square x) (* x x))

(define pi-estimate
  (lambda (n) ; that's the highest number in the numerator
    (define (next x) (+ 2 x))
    (* 4.0 (/ (* 2 (product square 4 next (- n 2)) n) (product square 3 next (- n 1))))))

(pi-estimate 10)
;3.3023935500125976
(pi-estimate 100)
;3.157339689217565
(pi-estimate 1000)
;3.143163842419198

;write an iterative version of product
(define product-iter
  (lambda (term a next b)
    (define iter
      (lambda (a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result)))))
    (iter a 1)))

(product-iter double 1 inc 3)
(product-iter double -1 inc 3)
(product-iter double 0 inc 0)
                              
                                                