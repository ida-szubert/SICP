#lang planet neil/sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-null? s) (null? s))
(define (the-empty-stream) '())

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream) (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (square x) (* x x))

;Infinite streams
;streams can be used to represent infinitely long sequences
;a normal list wouldn't do, since an infinitely long list would render any expression containing it effectively non-evaluable

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7))) integers))

(stream-ref no-sevens 100)
;117

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(stream-ref fibs 30)
;832040

;infinite stream of prime numbers using the sieve of Eratosthenes
;start with integers begining with 2, the first prime
;filter out multiples of 2 from the rest of the integers
;it leaves a strem begining with 3, the next prime
;filter out all multiples of 3 from the remaining integers
;ect

(define (sieve stream)
  (cons-stream (stream-car stream)
               (sieve (stream-filter (lambda (x)
                                       (not (divisible? x (stream-car stream))))
                                     (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(stream-ref primes 50)
;233

;Defining streams implicitly

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers2 (cons-stream 1 (add-streams ones integers)))

(define fibs2 (cons-stream 0 
                           (cons-stream 1 
                                        (add-streams (stream-cdr fibs) fibs))))
;generates the string of fibonacci numbers by saying that it starts with 0 1 and that the rest can be obtained by adding this string to itself
;but shifted by one place

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))
;this function produces the sequence of powers of 2
(stream-ref double 0)
(stream-ref double 1)
(stream-ref double 2)
(stream-ref double 3)
(stream-ref double 4)
(stream-ref double 5)

(define primes2
  (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))

;so, primes uses prime?, and prime? uses primes
;it works because at any point there are enought primes generated to test the primality of a given number n
;either n is not prime, in which case an already generated prime will divide it
;or it is a prime, and there's an already generated prime less then n, but greater than square root n

;Exercise 3.53
(define x (cons-stream 1 (add-streams x x)))
;each element will be the preveious element added to iself

;Exercise 3.54
(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers2))))
(stream-ref factorials 0)
(stream-ref factorials 1)
(stream-ref factorials 2)
(stream-ref factorials 3)
(stream-ref factorials 4)
(stream-ref factorials 5)

;Exercise 3.55
(define (partial-sums s)
  (define stream
    (cons-stream 0 (add-streams s stream)))
  (add-streams s stream))

(stream-ref (partial-sums integers2) 0)
(stream-ref (partial-sums integers2) 1)
(stream-ref (partial-sums integers2) 2)
(stream-ref (partial-sums integers2) 3)
(stream-ref (partial-sums integers2) 4)

;Exercise 3.56
;enumerate, in ascending order and with no repetitions, all positive integers with no prime factors other than 2, 3, or 5
;you could just test each integer in turn
;what's true of the required stream?
;begins with 1
;elements of (scale-stream s 2), (scale-stream s 3), and (scale-stream s 5) are also elements of s
;these are all the elements of s

;merge combines two ordered streams into one ordered stream, eliminating repetitions
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (merge (scale-stream S 2) (scale-stream S 3)) (scale-stream S 5))))

(stream-ref S 0)
(stream-ref S 1)
(stream-ref S 2)
(stream-ref S 3)
(stream-ref S 4)
(stream-ref S 5)
(stream-ref S 20)

;Exercise 3.57
;How many additions are performed by fibs2 to get the nth fibonacci number?
;n-1

;without memoization, we would need to construct fib (n-1) and fib (n-2)
;that's O(2^n)

;Exercise 3.58
(define (expand num den radix)
  (cons-stream (quotient (* num radix) den)
               (expand (remainder (* num radix) den) den radix)))
;expand produces an expansion of a rational number
;with a specified base
;stating from the first decimal point

(stream-ref (expand 1 7 10) 0);1
(stream-ref (expand 1 7 10) 1);4
(stream-ref (expand 1 7 10) 2);2
(stream-ref (expand 1 7 10) 3);8
(stream-ref (expand 1 7 10) 4);5

(stream-ref (expand 1 2 10) 0);5
(stream-ref (expand 1 2 10) 1);0
(stream-ref (expand 1 2 10) 2);0
(stream-ref (expand 1 2 10) 3);0
(stream-ref (expand 1 2 10) 4);0

(stream-ref (expand 3 8 10) 0);3
(stream-ref (expand 3 8 10) 1);7
(stream-ref (expand 3 8 10) 2);5
(stream-ref (expand 3 8 10) 3);0
(stream-ref (expand 3 8 10) 4);0

;Exercise 3.59
;power series
;things like cos x, sin x, e^x
;can be represented as infinite streams
;a0 + a1*x + a2*x^2 +a3*x^3 + a4*x^4 ...
;would be represented as a stream of coefficients

;a.
(define (div-streams s1 s2) (stream-map / s1 s2))

(define (integrate-series coeffs)
  (let ((fractions (div-streams ones integers)))
    (mul-streams coeffs fractions)))

(stream-ref (div-streams ones integers) 0)
(stream-ref (div-streams ones integers) 1)
(stream-ref (div-streams ones integers) 2)
(stream-ref (div-streams ones integers) 3)
(stream-ref (div-streams ones integers) 4)
  
;I want a stream 1 1/2 1/3 1/4 1/5 ...
;then i map mult over this stream and coeffs

;b
;the function x --> e^x is it's own derivative
;e^x and the integral of e^x are the same series
;except that the latter includes the constant, 1

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;the derivative of sine is cosine, and the derivative of cosine is the negative of sine

(define cosine-series (cons-stream 1 (stream-map (lambda (x) (* -1 x)) (integrate-series sine-series))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

;Exercise 2.60
(define (mul-series s1 s2)
  (let ((s1first (stream-car s1))
        (s2first (stream-car s2))
        (s1rest (stream-cdr s1))
        (s2rest (stream-cdr s2)))
  (cons-stream (* s1first s2first) (add-streams (scale-stream s2rest s2first)
                                                (scale-stream s1rest s2first)
                                                (mul-series s1rest s2rest)))))

(define test (add-streams (mul-series cosine-series cosine-series)
                         (mul-series sine-series sine-series)))

(stream-ref test 0)
;1

;Exercise 2.61
;S is a power series with the constant term 1
;we want to find the power series 1/S (i.e. such that S*X = 1)

(define (invert-unit-series s)
  (cons-stream 1 (stream-map (lambda (x) (* x -1)) (mul-series s (invert-unit-series s)))))

;Exercise 2.62
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      "Division by series with a constant term equal to 0"
      (let ((s2inverted (invert-unit-series s2)))
        (mul-series s1 s2inverted))))
;it's wrong, but I don't relly get why
;guess i don't know what does it mean to divide streams
  