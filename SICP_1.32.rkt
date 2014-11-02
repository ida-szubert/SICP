#lang planet neil/sicp


(define accumulate
  (lambda (combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b)))))

(define sum
  (lambda (term a next b)
  (+ 0 term a next b)))

(define product
  (lambda (term a next b)
  (accumulate * 1 term a next b)))

(define accumulate-iter
  (lambda (combiner null-value term a next b)
    (define iter
      (lambda (a result)
        (if (> a b)
            (result)
            (iter (next a) (combiner result (term a))))))
    (iter a null-value)))
                  
(define sum-iter
  (lambda (term a next b)
    (accumulate-iter + 0 term a next b)))

(define product-iter
  (lambda (term a next b)
    (accumulate-iter * 1 term a next b)))