#lang racket

;if
;n is prime
;a is any positive integer less the n
;then
;a^(n-1) is congruent to 1 modulo n
;i.e a^(n-1) divided by n has remainder 1

;pick a random number less then n
;use exmod
;but it needs to be modified to check, at each squaring step, if we've discovered a "non-trivial square root of 1 modulo n,"
;i.e., a number not equal to 1 or (n - 1) whose square is equal to 1 modulo n.

(define (square x) (* x x))

(define expmod
  (lambda (base exp m)
    (cond ((zero? exp) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m)))))

;the idea is to succesively reduce the exponent untill its 0
;because for whatever b, b^0 is 1, and for whatever m=/=1 (remainder 1 m) is 1
;we know we can reduce the exponent in this way beacuse of Fermat's Little Theorem
;(b^e modulo m) = ((b^e/2 modulo m)^2 modulo m)
;(b^e modulo m) = ((b^e-1 modulo m)*b modulo m)

;now, not to be fooled by Carmichael numbers, each time we square the (expmod b smaller-exp m) result, we check something about this square
;namely, we check if the square is not 1 or (n-1), and if square modulo n is 1
;if these conditions are fulfilled, the n is not prime

(define modified-expmod
  (lambda (base exp m)
    (cond ((zero? exp) 1)
          ((even? exp) (let ((tested-number (square (expmod base (/ exp 2) m))))
                         (if (and (eq? (remainder tested-number m) 1)
                                  (and (> tested-number 1) (< tested-number (- m 1))))
                             0
                             (remainder tested-number m))))
          (else (remainder (* base (expmod base (- exp 1) m)) m)))))

(define miller-rabin-test
  (lambda (n a)
    (= (modified-expmod a (- n 1) n) 1)))

(define (miller-rabin n)
   (define (iter counter)
     (cond ((= counter 1) #t)
           ((not (miller-rabin-test n counter)) #f)
           (else (iter (- counter 1)))))
   (iter (ceiling (/ n 2))))
                         
(miller-rabin 7)
(miller-rabin 561)
(miller-rabin 1105)
(miller-rabin 1729)
(miller-rabin 2465)
(miller-rabin 2821)
(miller-rabin 6601)

;Miller-Rabin is not fooled!
