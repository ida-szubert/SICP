#lang racket

;Exercise 1.42
(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

(define (inc x) (+ x 1))
(define (square x) (* x x))
((compose square inc) 6)
((compose inc square) 6)

;Exercise 1.43
(define repeated
  (lambda (f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1))))))


((repeated square 2) 5)
((repeated inc 10) 10)


;Exercise 1.44
(define smooth
  (lambda (f)
    (let ((dx 0.001))
     (lambda (x) (/ (+ (f(x)) (f (- x dx)) (f (+ x dx))) 3)))))

(define n-fold-smooth
  (lambda (f n)
    (repeated (smooth f) n)))


;Exercise 1.45
;finding fixed points works fine with average-damp for f(y) = x/y^2 and f(y) = x/y^3
;it doesn't for n>3
;to make it work, you need to modify the fixed-point procedure to include a number of repetitions of average-damp
;we don't yet know how many repetitions are needed with respect to n

(define fixed-point
  (lambda (f first-guess)
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) 0.00001))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess)))

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (cube x) (* x x x))
(define (4th-power x) (* x x x x))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (square y)))) 1.0))

(define (4th-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (cube y)))) 1.0))

(sqrt 4)
(cube-root 8)
(4th-root 16)
;even cubic root works better with repeated average-damp

(define (log2 x) (/ (log x) (log 2)))

(define nth-root
  (lambda (x n)
    (define (as-needed n) (floor (log2 n)))
    (fixed-point ((repeated average-damp (as-needed n)) (lambda (y) (/ x (expt y (- n 1))))) 1.0)))

(nth-root 16 4)
(nth-root 32 5)
(nth-root 64 6)
(nth-root 128 7)
(nth-root 256 8)
;2 repetitions of average-damp works up to the 7th root
(nth-root 512 9)
(nth-root 1014 10)
(nth-root 2048 11)
(nth-root 4096 12)
(nth-root 8192 13)
(nth-root 16384 14)
(nth-root 32768 15)
(nth-root 65536 16)
;3 repetitions of average-damp works up to the 15th root
;1 - 3
;2 - 7
;3 - 15
;when the number of the root changes from n to (+ (* 2 n) 1), we need to increase repetitions by 1
;if a is the number of repetitions, the maximum n which a supports is 2^(a+1) - 1
;how to calculate the needed number of average-damps knowing the value of n?
;n = 2^(a+1) - 1
;n + 1 = 2^(a+1)
;log2 (n + 1) = a + 1
;(log2 (n + 1)) - 1 = a

;scheme doesn't have a log2 function, so I need to define it
;using this to determine the number of repetitions doesn't work; we run out of memory
;that's because I don't know much about mathematics; I need to floor the resulting number to be sure to get an integer



