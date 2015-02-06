#lang planet neil/sicp
(#%require (only math/base random-natural))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-null? s) (null? s))
(define (the-empty-stream) '())
(define ones (cons-stream 1 ones))

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

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (partial-sums s)
  (define stream
    (cons-stream 0 (add-streams s stream)))
  (add-streams s stream))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (sieve stream)
  (cons-stream (stream-car stream)
               (sieve (stream-filter (lambda (x)
                                       (not (divisible? x (stream-car stream))))
                                     (stream-cdr stream)))))
(define (divisible? x y) (= (remainder x y) 0))

(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a x) c) m)))
(define random-init (random-natural 4000))

;Problems with explicit delay:
;we need to remember that, e.g. integral should be called with a delayed integrand stream
;and every procedure that uses integral must take it into account
;? couldn't we just take whatever stream, delay it as the first thing that's done by integral, and then force it when needed?
;hmm, maybe not, because on iteration we would have sth that's doubly delayed, if that's even possible

;anyway, that's a bad thing cause now there are procedures that take normal arguments, and procedures that take delayed arguments
;but are otherwise the same
;maybe we should make all procedures take delayed arguments?
;that would mean that every operation would use normal-order evaluation
;i.e. operands and not evaluated untill their values are needed
;instead, operand expressions are substituted for formal parameters, and evaluation is performed onece everything is fleshed out in this way
;it would be ok if we were concerned with tream processing only
;? although it was earlier shown that very often normal-order evaluation is less efficient than applicative-order evaluation

;delays are be bad for writing programs that use assignment, mutate data, or perform I/O


;Modularity of programs and objects
;an advantage of assignment is that modularity can be increased by encapsulating parts of the state of a larger system in local variables
;stream models can provide equivalent modularity
;Monte Carlo estimation of pi

;the point was to hide the internal state of random number generator from programs that use random numbers
;there was a rand-update procedure, whose internal changing varaible supplied random numbers
(define rand
  (let ((x random-init))
    (lambda ()
       (set! x (rand-update x))
       x)))

(define random-numbers
  (cons-stream random-init (stream-map rand-update random-numbers))) ;here's the stream of rundom numbers, with successive ones generated when needed

(define (map-successive-pairs f s)
  (cons-stream (f (stream-car s) (stream-car (stream-cdr s)))
               (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed))
                 (monte-carlo (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream);because e-s is a stream of #t and #f
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 (+ p 0.001)))) (monte-carlo cesaro-stream 0 0)))

;There's still a general monte-carlo procedure that can work with different experiments
;also, there's a pretty clear flow of information
;we've got a stream of random numbers
;we partition it into pairs and check sth about them
;thus generating a stream of booleans
;and then monte-carlo constructs a stream out of this strean of booleans
;(although I forgot what it does)


;Exercise 3.81
;A stream-based implementation of random, using rand-update
(define (random-stream n)
  (define (random-starting-with-x x)
    (cons-stream x (stream-map rand-update (random-starting-with-x x))))
  (cond ((eq? n 'generate)
         (cons-stream random-init (stream-map rand-update (random-stream 'generate))))
        ((eq? n 'reset)
         (lambda (x) (random-starting-with-x x)))))

(define generate-random-stream (random-stream 'generate))
(define (random-stream-starting-with x) ((random-stream 'reset) x))

;(display-stream generate-random-stream)
;118
;148
;7348
;7048
;9118
;12118
;3763
;10798
;7933
;11653
;3268
;3103
;538
;2188
;3148
;11338
;1738
;6253
;3493
;7723
;10633
;5368
;958

;(display-stream (random-stream-starting-with 118))
;118
;148
;7348
;7048
;9118
;12118
;3763
;10798
;7933
;11653
;3268
;3103
;538
;2188
;3148
;11338
;1738
;6253
;3493
;7723
;10633
;5368
;958


;Exercise 3.82
(define (square x) (* x x))

(define (monte-carlo-1 experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed))
                 (monte-carlo (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream);because e-s is a stream of #t and #f
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-in-range low high)
  (define (generate-random x)
    (let ((range (- high low)))
      (+ low (/ (random (* range 100)) 100))))
  (cons-stream (generate-random 1) (stream-map generate-random ones)))

(define (predicate x y)
    (<= (+ (square x) (square y)) 1))

(define (estimate-integral p x1 x2 y1 y2)
  (let ((square-area (* (abs (- x2 x1)) (abs (- y2 y1)) 1.0));1.0 is here to give me result in decimal rather than fraction
        (pi-estimate-stream (map-successive-pairs p (random-in-range x1 x2))))
    (scale-stream (monte-carlo pi-estimate-stream 0 0) square-area)))

(stream-ref (estimate-integral predicate -1 1 -1 1) 10000000)
;3.13624... the 100000th term
;that's not perfect...
;3.14165... the 10000000th term




