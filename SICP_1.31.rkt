#lang planet neil/sicp


;write a high-level procedure analogous to sumation, but outputing product rather than sum
(define product
  (lambda (term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b)))))

;define factorial in terms of product
(define (identity x) (x))
(define (inc x) (+ x 1))

(define factorial
  (lambda (n)
    (product identity 1 inc n)))

;compute an approximation of pi using formula from the book
(define (square x) (* x x))

(define pi-estimate
  (lambda (n) ; that's the highest number in the numerator
    (define (next x) (+ 2 x))
    (* 4 (/ (* 2 (product square 4 next (- n 2)) n) (product square 3 next (- n 1))))))

(pi-estimate 10)
(pi-estimate 100)
(pi-estimate 1000)
       

;write an iterative version of product
(define product-iter
  (lambda (term a next b)
    (define iter
      (lambda (a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result)))))
    (iter a 1)))
                              
                                                