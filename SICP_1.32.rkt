#lang planet neil/sicp

(define (double x) (* 2 x))
(define (inc x) (+ x 1))

(define accumulate
  (lambda (combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b)))))

(define sum
  (lambda (term a next b)
  (accumulate + 0 term a next b)))

(sum double 1 inc 3)

(define product
  (lambda (term a next b)
  (accumulate * 1 term a next b)))

(product double 1 inc 3)

(define accumulate-iter
  (lambda (combiner null-value term a next b)
    (define iter
      (lambda (a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (term a))))))
    (iter a null-value)))
                  
(define sum-iter
  (lambda (term a next b)
    (accumulate-iter + 0 term a next b)))

(sum-iter double 1 inc 3)

(define product-iter
  (lambda (term a next b)
    (accumulate-iter * 1 term a next b)))

(product-iter double 1 inc 3)