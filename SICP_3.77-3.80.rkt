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

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define (square x) (* x x))
(define ones (cons-stream 1 ones))


;Streams and delayed evaluation
;streams are good for modeling signal-processing systems with feedback loops
;because stream, due to delayed evaluatio, can be defined in terms of themselves

;but sometimes the "hidden" delay supplied by cons-stream is not enough
;so, here's a try at a program which solves the equation dy/dt = f(y), given y0
(define (first-solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)

;this doesn't work, because y used dy, which isn't yet defined, but the definition of dy has to come after y, because it uses y
;but primes and prime? worked ok with such a circular reference...
;yeah: "The intent of this definition does make sense, because we can begin to generate y without knowing dy"
;so, integral should expect the integrand stream to be a delayed argument
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt) int))))
  int)

(define (solve f y0 dt)
  (letrec ((y (lambda () (integral (delay dy) y0 dt)))
           (dy (lambda () (stream-map f y))))
    y))
;  (define y (integral (delay dy) y0 dt))
;  (define dy (stream-map f y))
;  y)

;(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
;It seems like DrRacket has a problem with recoursively referenced internal definitions
;"y: undefined, cannot use before initialization"
;even if I use letrec, which looks like it should work, there's a problem
;although, it's a different one
;"mcdr: contract violation; expected: mpair? given: #<procedure>

;Exercise 3.77
(define (integral2 integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? integrand)
                   the-empty-stream
                   (integral (stream-cdr integrand)
                             (+ (* dt (stream-car integrand)) initial-value)
                             dt))))
;This definition cannot be used in a system with loops, for the same reason that original integral woudn't work
(define (modified-integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (stream-cdr integrand)
                               (+ (* dt (stream-car integrand)) initial-value)
                               dt)))))


;Exercise 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)

;Exercise 3.79
;I have no idea what this notation means
;Instead of researching it, I'll just take a look at the solutions on-line to see what this question is about
;ok, so ddy is what is generalised - it a stream produced by appling f to dy and y

(define (general-solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;Exercise 3.80
;series RLC circuit
;R-resistance, L-inductance, C-capacitance

(define (RLC R L C dt)
  (lambda (v0 i0)
    (define v (integral (delay dv) v0 dt))
    (define dv (scale-stream i (/ -1 C)))
    (define i (integral (delay di) i0 dt))
    (define di (add-streams (scale-stream i (/ (* R -1) L)) (scale-stream v (/ 1 L))))
    (cons v i)))

(define circuit (RLC 1 0.2 1 0.1))
(define streams (circuit 10 0))
;(stream-ref (car streams) 10)
;(stream-ref (cadr streams) 10)
;great, cannot check it either, because "i: undefined; cannot use before initialization"



      