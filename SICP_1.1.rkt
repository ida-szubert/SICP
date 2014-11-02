#lang racket
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
(define sum-of-squares-largest
  (lambda (a b c)
    (cond ((and (> a c) (> b c)) (+ (* a a) (* b b)))
          ((and (> a b) (> c b)) (+ (* a a) (* c c)))
          (else (+ (* b b) (* c c))))))

(sum-of-squares-largest 2 3 4)
               
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

(define square
  (lambda (x)
    (* x x)))

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

;Exercise 1.7
; For very small numbers 0.001 is to large a number to allow for an accurate estimate
; it's not good enough to be within 0.001 of the right answer

; If x is large enough then the representations of numbers are not precise enough to be able to compare (square guess) to the third decimal figure
; good-enough? will never give an answer, because computer is unable to compare the numbers

(define sqrt2
  (lambda (x)
    (sqrt-iter2 0 1.0 x)))
    
(define sqrt-iter2
  (lambda (previous-guess guess x)
    (new-if (good-enough2? previous-guess guess)
            guess
            (sqrt-iter2 guess (improve guess x) x))))

(define good-enough2?
  (lambda (previous-guess guess)
    (< (abs (- previous-guess guess)) 0.001)))

;(sqrt2 9999999999998)
;still, program run out of memory


;Exercise 1.8
(define cubrt
  (lambda (x)
    (cubrt-iter 1 x)))

(define cubrt-iter
  (lambda (guess x)
    (if (cub-good-enough? guess x)
        guess
        (cubrt-iter (cub-improve guess) x))))

(define cub-improve
  (lambda (guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3)))

(define cub-good-enough?
  (lambda (guess x)
    (< (abs (- (cube guess) x)) 0.001)))

(define cube
  (lambda (x)
    (* x x x)))

;Exercise 1.9
;recursive addition
;(define +
; (lambda (a b)
;    (if (= a 0) b (inc (+ (dec a) b)))))
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
(define A
  (lambda (x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1) (A x (- y 1)))))))

(A 1 10)
;2^10  
(A 2 4)
;A 1 (A 2 3) = (A 1 16) = 2^16
;(A 2 3) = (A 1 (A 2 2)) = (A 1 4) = (A 0 (A 1 3) = (*2 (A 0 (A 1 2)) = * 2 2 4 = 16
;(A 2 2) = (A 1 (A 2 1)) = (A 1 2) = (A 0 (A 1 1)) = (A 0 2) = 4
(A 3 3)
; 2^16 (65536)


(define f
  (lambda (n)
    (A 0 n)))
;2n

(define g
  (lambda (n)
    (A 1 n)))
;2^n

(define h
  (lambda (n)
    (A 2 n)))
;2^(h (- n 1))

(define k
  (lambda (n)
    (* 5 n n)))
;5n^2

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
    (cond ((< col row) #f)
          ((or (zero? col) (= row col)) 1); left and right egde of the triangle
          (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))))

;Exercise 1.13
;TODO


;Exercise 1.14
;space: O(n)
;The space required is proportional to the maximum depth of the tree.
;The maximum depth is reached on (cc 1 1).
;Once we get to the point where (= kinds-f-coins 1) then with every step we decrease the amount by 1
;It takes (amount) steps to reach maximum depth 
;The growth is linear with respect to amount
;
;number of steps: 
;There's a lot of redundancy in computation, like in the Fibbonaci sequence algorithm
;(cc 6 1) and (cc 1 2) are computed twice
                  
                  
                  
;Exercise 1.15
(define cube2
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

;for 12.50
;(p (sine 4.05))
;(p (p (sine 1.35)))
;(p (p (p (sine 0.45))))
;(p (p (p (p (sine 0.15)))))
;(p (p (p (p (p (sine 0.05))))))
;(p (p (p (p (p 0.05)))))
;-0.39980345741334


;Exercise 1.16
(define exp-iter
  (lambda (b n)
    (if (= n 1)
        b
        (exp-iter (* b b) (- n 1)))))

(define exp
  (lambda (b n)
    (exp-help 1 b n)))

(define exp-help
  (lambda (a b n)
    (cond ((= n 0) a)
          ((even? n) (exp-help a (square b) (/ n 2)))
          (else (exp-help (* a b) b (- n 1)))))) 
                      

;Exercise 1.17
(define double
  (lambda (x)
    (* 2 x)))

(define halve
  (lambda (x)
    (/ x  2)))

(define mult
  (lambda (a b)
    (cond ((or (zero? b) (zero? a)) 0)
          ((= b 1) a)
          ((even? b) (mult (double a) (halve b)))
          (else (+ a (mult a (- b 1)))))))


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
            (russian-help (double a) (halve b) acc)
            (russian-help (double a) (halve b) (+ acc a)))))) 
                    


;Exercise 1.20
(define gcd
  (lambda (a b)
    (if (zero? b)
        a
        (gcd b (remainder a b)))))

;normal-orde evaluation: expand the expression fully before evaluating it

(if (= 40 0)
    206
    (gdc  40 (remainder 206 40)))

(if (= (remainder 206 40) 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remiander 206 40))))

;evaluate one remainder
(if (= 6 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remiander 206 40))))

(if (= (remainder 40 (remiander 206 40)) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remiander 206 40)) (remainder
                                            (remainder 206 40)
                                            (remainder 40 (remiander 206 40)))))
;evaluate 2 nested remainders
(if (= 4 0)
    (remainder 206 40)
    (gcd (remainder 40 (remiander 206 40)) (remainder
                                            (remainder 206 40)
                                            (remainder 40 (remiander 206 40)))))

(if (= (remainder
        (remainder 206 40)
        (remainder 40 (remiander 206 40))) 0)
    (remainder 40 (remiander 206 40))
    (gcd (remainder
          (remainder 206 40)
          (remainder 40 (remiander 206 40))) (remainder (remainder 40 (remiander 206 40)) (remainder
                                                                                         (remainder 206 40)
                                                                                         (remainder 40 (remiander 206 40))))))
;evaluate 4 nested remainders
(if (= 2 0)
    (remainder 40 (remiander 206 40))
    (gcd (remainder
          (remainder 206 40)
          (remainder 40 (remiander 206 40))) (remainder (remainder 40 (remiander 206 40))
                                                        (remainder
                                                         (remainder 206 40)
                                                         (remainder 40 (remiander 206 40))))))
                                                                                         
                                                                                         
(if (= (remainder (remainder 40 (remainder 206 40))
                  (remainder (remainder 206 40)
                             (remainder 40 (remainder 206 40)))) 0)
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40)))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))

;evaluate 7 nested remainders
(if (= 0 0)
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40)))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))

;calculate remainders in the then-clause, i.e 4 remainders
;total of 18 calculations
          

;applicative-order evaluation: evaluate all the arguments beforehand
(gcd 206 40)
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

;one remainder calculated
(gcd 40 6)
(if (= 6 0)
    40
    (gcd 6 (remainder 40 6))) 

;one remainder calculated
(gcd 6 4)
(if (= 4 0)
    6
    (gcd 4 (remainder 6 4)))

;one remainder calculated
(gcd 4 2)
(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))

;one remainder calculated
(gcd 2 0)
(if (= 0 0)
    2
    (gcd 0 (remainder 2 0)))

;no more remainders calculated; total of 4 calculations


;Exercise 1.21
           























          
                       
          

