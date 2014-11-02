#lang planet neil/sicp
;STREAMS
;assignment is useful, but causes problems
;is there an alternative?
;is it necessary to make the model change in time in order to mdoel phenomena in a changing world?

;the tme-varying behaviour of quantity x can be described as a function of time x(t)
;you can concentrate on x instant by instant, and then it looks like a changing wuantity
;but you can also look at the entire history of values and see a function which does not change
;If time is measured in discrete steps, a time function can be modeled as a sequence (stream are sequences)

;Lists
;we've got a lot of functions for manipulating lists
;but, if sequence manipulations are represented as list transformations, inefficiency with respect to both time and spce will result
;programs would have to construct and copy data structures at every step of a process

;computing the sum of all primes in an interval:
;standard iterative style
;(define (sum-primes a b)
;  (define (iter count accum)
;    (cond ((> count b) accum)
;          ((prime? count) (iter (+ count 1) (+ count accum)))
;          (else (iter (+ count 1) accum))))
;  (iter a 0))

;using sequence operations
;(define (sum-primes2 a b)
;  (accumulate + 0 (filter prime? (enumerate-interval a b))))

;first program needs to store the sum being accumulated
;second program needs to generate a list of numbers in the interval and pass it to filter
;and to generate a list of filtered numbers
;which is passed to accumulate and then summarized
;so, large intermediate storage is needed
;also, list manipulations can be extremely inefficient in some circumstances
;e.g. you wouldn't want to enumerate all integers between 10 000 and  1 000 000, test all for primality, and feed the result to cadr
;only to get the second prime in the interval
;it's just an awful lot of unnecessary work
;the solution is to construct streams partially, and to pass this part to a program that consumes this stream
;if this program wants more, the tream will construct more of itself, just waht's needed
;we keep the ilusion that the whole stream exists
;and write programs as if it were true
;but if fact the construction of the stream is interleaved with its use

;streams are represented with operations cons-stream, stream-car, stream-cdr, the-empty-stream, stream-null?
;it's easy to build analogs of list operations
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

;the fact that list and stream operations are so similar is suspicious
;does it not beg for abstraction?
;apparently it does, but to exploit this situation we need "finer control over the process of evaluation"

;cdr of a stream should be evaluated when it is accessed by the stream-cdr rather then when the stream is constructed by cons-stream
;the difference between streams and lists is thus the time at which the elements are evaluated
;cons-stream will be a special form, using the special form (delay <x>)
;(cons-stream <a> <b>) == (cons <a> (delay <b>))
;which means that stream-cdr will only be evaluated when needed
;to evaluate an expression whose evaluation was delayed, the function force is used
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;demand-driven programming style

;Implementing delay and force
;delay must packagean expression so that it can be evaluated on demand
;the expression can be treated as the body of a procedure
;so the special form (delay <exp>) is syntacic sugar for (lambda () <exp>)
;force, in turn, calls the procedure produced by delay
(define (force delayed-object) (delayed-object))

;in many applications we force the same delayed-object many times
;so, delayed objects should be such that they store the value that's computed the first time they are forced
;on the enxt call to force nothing will be computed, but a ready value will be recalled

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

;delay is then defined so that (delay <exp>) == (memo-proc (lambda () <exp>))

;Exercise 3.50
;map which takes a prcedure of n arguments and n streams, and applies the procedure to all the cars of the streams, and so on, returning one stream
(define (general-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply general-stream-map (cons proc (map stream-cdr argstreams))))))

