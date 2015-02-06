#lang planet neil/sicp

; Exercise 1.1
10
;10
(+ 5 3 4)
;12
(- 9 1)
;8
(/ 6 2)
;3
(+ (* 2 4) (- 4 6))
;6
(define a 3)
;a
(define b (+ a 1))
;b
(+ a b (* a b))
;19
(= a b)
;#f
(if (and (> b a) (< b (* a b)))
    b
    a)
;4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;16
(+ 2 (if (> b a) b a))
;6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;16


;Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;Exercise 1.3
(define (square x) (* x x))

(define sum-of-squares-largest
  (lambda (a b c)
    (cond ((and (> a c) (> b c)) (+ (square a) (square b)))
          ((and (> a b) (> c b)) (+ (square a) (square c)))
          (else (+ (square b) (square c))))))

(define sum-of-squares-largest-2
  (lambda (a b c)
    (if (> a c) 
        (if (> b c)
            (+ (square a) (square b))
            (+ (square a) (square c)))
        (if (> b a)
            (+ (square c) (square b))))))

(sum-of-squares-largest 2 3 4) ;25
(sum-of-squares-largest-2 2 3 4) ;25
               
;Exercise 1.4
;The procedure checks whether b is greater than 0. If it is, we proceed to add a and b, if it is not, we proceed to subtract b from a

;Exercise 1.5
;(define (p) (p))
;(define (test x y)
;  (if (= x 0) 0 y))

;(test 0 (p)) 

;applicative-order evaluation:
              ;we'd like to evaluate the arguments, but evaluation of (p) never ceases
              ;aplicative-order evaluation of (test 0 (p)) never ceases

;normal-order evaluation:
              ;we substitute 0 and (p) for x and y 
;((if (= 0 0) 0 (p)))
              ;we don't need to evaluate (p) to get the final answer
              ;there are already only primitive operators involved, so the answer can be obtained
              ;the answer is 0 and we needn't worry about the fact that (p) is not evaluable
;btw, DrRacket uses applicative-order evaluation



;Exercise 1.6
(define new-if
  (lambda (predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause))))

(define improve
  (lambda (guess x)
    (/ (+ guess (/ x guess)) 2)))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) 0.001)))

(define sqrt-iter
  (lambda (guess x)
    (new-if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x) x))))

;(sqrt-iter 1 4)
; for some reason execution never finishes when new-if is used
; ultimately that's because of applicative-order evaluation
; new-if is a function, and all its arguments should be evaluated before the whole application is evaluated
; but the else-clause will keep evaluating indefinitely
; if, on the other hand, is a special form- when (good-enough? guess x) is #t, the else-clause will not be evaluated at all

;Exercise 1.7
; For very small numbers 0.001 is too large a number to allow for an accurate estimate
; it's not good enough to be within 0.001 of the right answer

; If x is vary large then the representations of numbers are not precise enough to be able to compare (square guess) to the third decimal figure
; good-enough? will never give an answer, because computer is unable to compare the numbers with the given precision

(define sqrt
  (lambda (x)
    (sqrt-iter2 0 1.0 x)))
    
(define sqrt-iter2
  (lambda (previous-guess guess x)
    (if (good-enough2? previous-guess guess)
            guess
            (sqrt-iter2 guess (improve guess x) x))))

(define good-enough2?
  (lambda (previous-guess guess)
    (< (abs (- previous-guess guess)) 0.001)))

(sqrt 9999999999998) ;3162277.660168063 
;when sqrt-iter2 uses new-if, evaluation never stops


;Exercise 1.8
(define cubrt
  (lambda (x)
    (cubrt-iter 1 x)))

(define cubrt-iter
  (lambda (guess x)
    (if (cub-good-enough? guess x)
        guess
        (cubrt-iter (cub-improve guess x) x))))

(define cub-improve
  (lambda (guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3)))

(define cub-good-enough?
  (lambda (guess x)
    (< (abs (- (cube guess) x)) 0.001)))

(define cube
  (lambda (x)
    (* x x x)))

(cubrt 8)
;reasonable- 2.000004911675504



;Exercise 1.9
;recursive addition
;(define +
; (lambda (a b)
;    (if (= a 0)
;        b
;        (inc (+ (dec a) b)))))

;(+ 4 5)
;(inc (+ 3 5))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc (5)))))
;(inc (inc (inc (6))))
;(inc (inc (7)))
;(inc 8)
;9
 
; iterative addition
;(define plus
;  (lambda (a b)
;    (if (= a 0) b (plus (dec a) (inc b)))))

;(+ 4 5)
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(+ 0 9)
;9


;Exercise 1.10
;Ackermann's function
(define Ac
  (lambda (x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (Ac (- x 1) (Ac x (- y 1)))))))

(Ac 1 10)
;2^10  
(Ac 2 4)
;A 1 (A 2 3) = (A 1 16) = 2^16
;(A 2 3) = (A 1 (A 2 2)) = (A 1 4) = (A 0 (A 1 3) = (*2 (A 0 (A 1 2)) = * 2 2 4 = 16
;(A 2 2) = (A 1 (A 2 1)) = (A 1 2) = (A 0 (A 1 1)) = (A 0 2) = 4
(Ac 3 3)
; 2^16 (65536)


(define f
  (lambda (n)
    (Ac 0 n)))
;2n

(define g
  (lambda (n)
    (Ac 1 n)))
;2^n

(define h
  (lambda (n)
    (Ac 2 n)))
;2^(h (- n 1)
(h 1) ;2
(h 2) ;4
(h 3) ;16
(h 4) ;65536
(h 5) ;HUGE


(define k
  (lambda (n)
    (* 5 n n)))
;5 n^2


;Exercise 1.11

;recursive process
(define foo
  (lambda (n)
    (cond ((< n 3) n)
          (else (+ (foo (- n 1)) (* 2 (foo (- n 2))) (* 3 (foo (- n 3))))))))

;iterative process
(define bar
  (lambda (n)
    (bar-iter 2 1 0 n)))

(define bar-iter
  (lambda (a b c count)
    (if (< count 3)
        a
        (bar-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
;if n = 3, then new variables will be 4 2 1 2
;at the beggining of every step, variable a stores the final result for (count + 1)



;Exercise 1.12
(define pascal
  (lambda (row col)
    (cond ((> col row) #f)
          ((or (zero? col) (= row col)) 1); left and right egdes of the triangle
          (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))))

(pascal 1 1);1
(pascal 2 1);2
(pascal 4 1);4
(pascal 4 2);6


;Exercise 1.13

;psi = (1 - sqrt 5) / 2
;phi = (1 + sqrt 5) / 2
;phi^n / sqrt 5 ---> fib(n) is the closes integer

;fib(n) = (phi^n - psi^n) / sqrt 5
;fib(n) * sqrt 5 = phi^n - psi^n
;(fib(n) * sqrt 5) + psi^n = phi^n
;fib(n) + (psi^n / sqrt 5) = phi^n / sqrt 5
;let's look at (psi^n / sqrt 5)
;((1 - sqrt 5) / 2)^n / sqrt 5
;(-1.236 / 2)^n / sqrt 5
;-0.618^n / sqrt 5
;the absolute value will always be less then 0.5, the smaller the greater n is
;which means that fib(n) is the closest integer to (phi^n / sqrt 5) 

;TODO 
;fib(n) = (phi^n - psi^n) / sqrt 5 is actually something that I'm supposed to prove
;fib(n) = (phi^n / sqrt 5) - (psi^n / sqrt 5)

(define fib
  (lambda (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1)) (fib (- n 2)))))))



;Exercise 1.14
;space: O(n)
;The space required is proportional to the maximum depth of the tree.
;The maximum depth is reached on (cc 1 1).
;Once we get to the point where (= kinds-f-coins 1) then with every step we decrease the amount by 1
;It takes (amount) steps to reach maximum depth 
;The growth is linear with respect to amount

;number of steps: 
;There's a lot of redundancy in computation, like in the Fibbonaci sequence algorithm
;(cc 6 1) and (cc 1 2) are computed twice
           























          
                       
          

