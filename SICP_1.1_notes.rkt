#lang racket
(define sqrt
  (lambda (x)
    (sqrt-iter 1.0 x)))

(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x))))

(define improve
  (lambda (guess x)
    (/ (+ guess (/ x guess)) 2)))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

(define square
  (lambda (x)
    (* x x)))




(define fib
  (lambda (n)
    (fib-iter 1 0 n)))
(define fib-iter
  (lambda (a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))



(define count-change
  (lambda (amount)
    (cc amount 5)))

(define cc
  (lambda (amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount (- kinds-of-coins 1))
                   (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins))))))

(define first-denomination
  (lambda (kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50))))

;e.g. we've got 3 kinds of coins
;we calculate the number of ways to change amount x using 2 kinds of coins
;and add this to the number of additional ways to change x using all 3 kinds of coins
;apparently the latter equals to the number of ways to change (x - the value of the highest coin) using 3 kinds of coins
;the first subexpression of + reduces kinds-of-coins to 0
;the second subexpression 




(define exp
  (lambda (b n)
    (if (zero? n)
        1
        (* b (exp b (- n 1))))))

(define expt
  (lambda (b n)
    (expt-iter b n 1)))

(define expt-iter
  (lambda (base counter product)
    (if (zero? counter)
        product
        (expt-iter base (- counter 1) (* base product)))))

(define fast-expt
  (lambda (b n)
    (cond ((zero? n) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1)))))))

(define even?
  (lambda (n)
    (= (remainder n 2) 0)))



(define gcd
  (lambda (a b)
    (if (zero? b)
        a
        (gcd b (remainder a b)))))
           
           
           
           
           
           
           
         
    