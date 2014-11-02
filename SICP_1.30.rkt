#lang planet neil/sicp

(define sum
  (lambda (term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b)))))


(define sum-iter
  (lambda (term a next b)
    (define iter
      (lambda (a result)
        (if (> a b)
            (result)
            (iter (next a) (+ result (term a))))))
    (iter a 0)))
                  
            
