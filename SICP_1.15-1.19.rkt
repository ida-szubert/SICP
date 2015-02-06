#lang racket
;Exercise 1.15
(define cube
  (lambda (x)
    (* x x x)))

(define p
  (lambda (x)
    (- (* 3 x) (* 4 (cube x)))))

(define sine
  (lambda (angle)
    (if (not (> (abs angle) 0.1))
        angle
        (p (sine (/ angle 3.0))))))

;for (sine 12.15)
;(p (sine 4.05))
;(p (p (sine 1.35)))
;(p (p (p (sine 0.45))))
;(p (p (p (p (sine 0.15)))))
;(p (p (p (p (p (sine 0.05))))))
;(p (p (p (p (p 0.05)))))
;-0.39980345741334
(sine 12.15)
(sine 1) ;0.84159456

;O(n)
;more precisely, a 3-fold increase of a means that sine performs an additional step
;this will happen often for small a, and not so often for larger a

;Exercise 1.16
(define (square x) (* x x))

(define exp-iter
  (lambda (b n)
    (define (help x y)
        (if (= y 1)
            x
            (help (* x b) (- y 1))))
    (if (>= n 0)
        (help b n)
        (/ 1 (help b (abs n))))))

(define exp
  (lambda (b n)
    (if (>= n 0)
        (exp-help 1 b n)
        (/ 1 (exp-help 1 b (abs n))))))

(define exp-help
  (lambda (a b n)
    (cond ((= n 0) a)
          ((even? n) (exp-help a (square b) (/ n 2)))
          (else (exp-help (* a b) b (- n 1)))))) 

(exp 3 3)
(exp 5 4)
(exp 12 2)
(exp 0 3)
(exp 2 -1)

(exp-iter 3 3)
(exp-iter 5 4)
(exp-iter 12 2)
(exp-iter 0 3)
(exp-iter 2 -1)                      


;Exercise 1.17
(define double
  (lambda (x)
    (* 2 x)))

(define halve
  (lambda (x)
    (/ x 2)))

(define mult
  (lambda (a b)
    (cond ((or (zero? b) (zero? a)) 0)
          ((= b 1) a)
          ((= a 1) b)
          ((even? b) (mult (double a) (halve b)))
          (else (+ a (mult a (- b 1)))))))

(mult 4 5)
(mult 1 -2)
(mult 0 6)
      

;Exercise 1.18

;  double a and halve b
;  check if the new b is even
;  if so, don't add the value of a to the accumulator
;  if b is odd, add the value of a to the accumulator
;  repeate the process with the new a and new b
;  stop when b = 1, and return a + accumulator

(define russian
  (lambda (a b)
    (russian-help a b 0)))

(define russian-help
  (lambda (a b acc)
    (if (= b 1)
        (+ a acc)
        (if (even? b)
            (russian-help (double a) (floor (halve b)) acc)
            (russian-help (double a) (floor (halve b)) (+ acc a))))))

(russian 5 7)
(russian 7 5)
(russian 0 20)
(russian -5 6)
                    

;Exercise 1.19
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;T is a transformation a <-- a+b, b <-- a
;applying it n times, starting with 1 and 0, produces the pair fib(n+1) and fib(n)
;fibonacci numbers are produces by applying the nth power of T, starting with (1, 0)
;let's think of T as a special case in a family of transformations transforming (a, b) like so:
;a <-- bq+aq+ap    b <-- bp+aq

;what does it mean to raise a transformation to some power?
;to find such p and q that, when substituted to the transformation formula, will give the same result that applying the original
;transformation n times would give
;let's examine squareing

;applying once:
;new a: (bq+aq+ap)
;new b: (bp+aq)

;applying twice:
;new a:
;((bp+aq)*q + (bq+aq+ap)*q + (bq+aq+ap)*p)
;(bpq + aq^2 + bq^2 + aq^2 + apq + bqp + aqp + ap^2)
;(2bpq + 2apq + 2aq^2 + bq^2 + ap^2)
;b*(2pq + q^2) + a*(2pq + q^2) + a*(q^2 + p^2)
;so, q' is (2pq + q^2)
;and p' is (q^2 + p^2)

;new b:
;((bp+aq)*p + (bq+aq+ap)*q)
;(bp^2 + aqp + bq^2 + aq^2 + apq)
;(bp^2 + bq^2 + aq^2 + 2apq)
;b*(q^2 + p^2) + a*(2pq + q^2)
;so, q' is (2pq + q^2)
;and p' is (q^2 + p^2)

(define (new-fib n)
  (new-fib-iter 1 0 0 1 n))

(define (new-fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (new-fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (new-fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;new-fib uses (1,0) as the starting pair and (0,1) as the starting coefficients
;if n is even, new-fib-iter applies T^2 instead of transformation, and so performs one step instead of two

(fib 4)
(new-fib 4)
(fib 25)
(new-fib 25)
