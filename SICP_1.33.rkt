#lang racket
;using a filter
;combining only those terms in the given range which satisfy a specified condition

(define filtered-accumulate
  (lambda (filter combiner null-value term a next b)
    (if (> a b)
        null-value
        (if (filter a)
            (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b))
            (filtered-accumulate filter combiner null-value term (next a) next b)))))

(define sum-of-squares-prime
  (lambda (a b)
    (filtered-accumulate (prime? + 0 square a inc b))))
     
(define (square x) (* x x))  
(define (inc x) (+ x 1))

(define prime?
  (lambda (n)
    (= n (smallest-divisor n))))

(define smallest-divisor
  (lambda (n)
    (find-divisor n 2)))

(define find-divisor
  (lambda (n test-divisor)
    (define (divides? a b)
      (= (remainder b a) 0))
    (define (next x)
      (if (= n 2)
          3
          (+ n 2)))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor))))))


;0 to n
;(gcd a n) = 1
(define product-of-coprimes
  (lambda (n)
    (define coprime?
     (lambda (i)
       (= (gcd i n) 1))) 
    (filtered-accumulate coprime? * 1 identity 1 inc (- n 1))))

(define gcd
  (lambda (a b)
    (if (= b 0)
        a
        (gcd b (remainder a b)))))

(product-of-coprimes 12)