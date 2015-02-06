#lang planet neil/sicp
(#%require (only math/base random-natural))

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
 
;Exercise 1.21
(smallest-divisor 199)
;199
(smallest-divisor 1999)
;1999
(smallest-divisor 19999)
;7

;Exercise 1.22

(define timed-prime-test
  (lambda (n)
    (newline)
    (display n)
    (start-prime-test n (runtime))))

(define start-prime-test
  (lambda (n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time)))))

(define report-prime
  (lambda (elapsed-time)
    (display " *** ")
    (display elapsed-time)))


(timed-prime-test 7)

;write a procedure that looks for primes among odd integers in a given range        
(define search-for-primes
  (lambda (lower upper)
    (if (even? lower)
        (search-for-primes (+ lower 1) upper)
        (cond ((< lower upper) (timed-prime-test lower)
                               (search-for-primes (+ lower 2) upper))))))

;noteworthy: you can specify two actions that need to be done if some condition if fulfiled
;whenever the number is odd and lower < upper, I ask for timed-prime-test to be performed on lower,
;and for search-for-primes to continue with new value of lower
;I don't keep the results anywhere, only print primes on screen                              


;I don't get any meaningful time reading below 10^8; all times are either 0 or 1000
;(search-for-primes 100000000000 100000000100)
;lower = 10^8
;100000007 *** 8000 (0)
;100000037 *** 14000 (0)
;100000039 *** 10000 (0)
;average = 10666 (0)

;lower = 10^9
;1000000007 *** 59000 (15000)
;1000000009 *** 126000 (16000)
;1000000021 *** 52000 (15000)
;average = 79000 (15333)

;lower = 10^10
;10000000019 *** 145000 (31000)
;10000000033 *** 138000 (16000)
;10000000061 *** 173000 (31000)
;average = 152000 (26000)

;lower = 10^11
;100000000003 *** 321000 (47000)
;100000000019 *** 416000 (31000)
;100000000057 *** 274000 (47000)
;average = 337000 (41666) 109000



(* 1.0 (/ 337000 152000)) ;=2,217
(* 1.0 (/ 152000 79000)) ;=1,924
(* 1.0 (/ 79000 10666)) ;=7,407
;these numbers are not close to 3,162 (sqrt 10)
;but there's huge variability (why, actually?)
;maybe if I averaged more than 3 values for every range I would get more reliable results
;but then the more values from range a I look at, the closer I'm getting to range b

;on another occasion:
(* 1.0 (/ 109000 26000)) ;=4,192
(* 1.0 (/ 26000 15333)) ;=1,695


;Exercise 1.23
(define next
  (lambda (n)
    (if (= n 2)
        3
        (+ n 2))))

(define fast-smallest-divisor
  (lambda (n)
    (fast-find-divisor n 2)))

(define fast-find-divisor
  (lambda (n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (fast-find-divisor n (next test-divisor))))))

(define fast-prime?
  (lambda (n)
    (= n (fast-smallest-divisor n))))

(define fast-timed-prime-test
  (lambda (n)
    (newline)
    (display n)
    (fast-start-prime-test n (runtime))))

(define fast-start-prime-test
  (lambda (n start-time)
    (if (fast-prime? n)
        (report-prime (- (runtime) start-time)))))


;timed-prime-test                  fast-timed-prime-test       ratio
;100000007 *** 2000                1000                        2
;100000037 *** 1000                1000                        1
;100000039 *** 1000                1000                        1

;lower = 10^9
;1000000007 *** 4000               2000                        2
;1000000009 *** 4000               3000                        1.33
;1000000021 *** 4000               2000                        2

;lower = 10^10
;10000000019 *** 12000             7000                        1.71
;10000000033 *** 12000             8000                        1.5
;10000000061 *** 12000             8000                        1.5

;lower = 10^11
;100000000003 *** 36000            23000                       1.56
;100000000019 *** 36000            24000                       1.5
;100000000057 *** 36000            23000                       1.56

;one reason for why timed-prime-test using next in not twice as fast might be that instead of a primitive operation +
;we are using a non-primitive procedure
;every time next is invoked, the if-clause must be tested, and only then will (+ n 2) be performed



;Exercise 1.24
(define expmod
  (lambda (base exp m)
    (cond ((zero? exp) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m)))))

(define fermat-test
  (lambda (n)
    (define try-it
      (lambda (a)
        (= (expmod a n n) a)))
    (try-it (+ 1 (random-natural (- n 1))))))


(define fermat-prime?
  (lambda (n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fermat-prime? n (- times 1)))
          (else #f))))


(define fermat-timed-prime-test
  (lambda (n)
    (newline)
    (display n)
    (fermat-start-prime-test n (runtime))))

(define fermat-start-prime-test
  (lambda (n start-time)
    (if (fermat-prime? n 100)
        (report-prime (- (runtime) start-time)))))


(fermat-timed-prime-test 100000007)
(fermat-timed-prime-test 100000037)
(fermat-timed-prime-test 100000039)
(fermat-timed-prime-test 1000000007)
(fermat-timed-prime-test 1000000009)
(fermat-timed-prime-test 1000000021)
(fermat-timed-prime-test 10000000019)
(fermat-timed-prime-test 10000000033)
(fermat-timed-prime-test 10000000061)
(fermat-timed-prime-test 100000000003)
(fermat-timed-prime-test 100000000019)
(fermat-timed-prime-test 100000000057)
 
;the problem is that random only works with n < 4294967087
;random-natural solves the problem
;still, it's difficult to answer the question, since times are either 0 or 15000, and not stable
