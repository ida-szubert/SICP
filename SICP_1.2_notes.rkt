#lang racket
;simple method
(define (square x)
  (* x x))

(define smallest-divisor
  (lambda (n)
    (find-divisor n 2)))

(define find-divisor
  (lambda (n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1))))))

(define divides?
  (lambda (a b)
    (= (remainder b a) 0)))

(define prime?
  (lambda (n)
    (= n (smallest-divisor n))))

;Fermat test
;first write a function that computes the remainder of (/ a^b b)
;to find remainder (x*y) modulo z you can find remainder x z and remainder y z, multiply them, and find the remainder of the result z 
;when b is even, find the remainder of a^(b/2) modulo b and square it (i.e. you are finding remainders x z and y z, only that x and y are the same in this case)
;the find the remainder of this product modulo b
;it will be the same as the remainder a^b modulo b

(define expmod
  (lambda (base exp m)
    (cond ((zero? exp) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m))))
          (else (remainder (* base (expmod base (- exp 1) m)) m)))))

;write a function which takes a number a and checks whether a^n modulo n equals a
;that's test-it
;write a function which takes a number n, chooses a random number less than n, and tests it
;(random (- n 1)) outputs a random number in the range from 0 to n-1
;but we want to test numbers >1, so we add 1
;so the higher limit of the numbers we're generating is n
(define fermat-test
  (lambda (n)
    (define try-it
      (lambda (a)
        (= (expmod a n n) a)))
    (try-it (+ 1 (random (- n 1))))))

;we want to run the test a number of times
;if a number is prime, then every run of (fermat-test n) will return #t, no matter the value of a
;if n is not prime, (fermat-test n) can be true, but only for some values of a

(define fast-prime?
  (lambda (n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f))))
         
    
(prime? 7)
(smallest-divisor 19999)
(/ 19999 7)
(smallest-divisor 2857)
