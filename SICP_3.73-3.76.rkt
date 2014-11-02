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

(define primes (sieve (integers-starting-from 2)))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))
(define (divisible? x y) (= (remainder x y) 0))
(define (square x) (* x x))
(define ones (cons-stream 1 ones))

;Streams as signals
;values of a signal at successive time intervals can be represented as consecutive elements of a stream

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt) int)))
  int)

;Exercise 3.73
(define (RC R C dt)
  (define (helper i v0)
    (cons-stream v0
                 (add-streams (scale-stream i R)
                             (integral (scale-stream i (/ 1 C)) v0 dt))))
  helper)

(define RC1 (RC 5 1 0.5))
(define constant-current (RC1 ones 0))
(stream-ref constant-current 0)
(stream-ref constant-current 1)
(stream-ref constant-current 2)
(stream-ref constant-current 3)
(stream-ref constant-current 4)
(stream-ref constant-current 5)
(stream-ref constant-current 6)


;Exercise 3.74
;the resulting signal should be +1 whenever the input signal changes from negative to positive,
;-1 when input changes from positive to negative,
;and 0 otherwise
;assuming that 0 in input is treated as a positive value
;sign-change-detector takes two arguments, compares the signs, and returns +1, -1, or 0

(define (sign-change-detector x y)
  (cond ((and (< x 0) (>= y 0)) +1)
        ((and (>= x 0) (< y 0)) -1)
        (else 0)))

(define (make-zero-crossings input last-value)
  (cons-stream (sign-change-detector (stream-car input) last-value)
               (make-zero-crossings (stream-cdr input) (stream-car input))))

;(define zero-crossings
;  (make-zero-crossings sense-data 0))
(define (general-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply general-stream-map (cons proc (map stream-cdr argstreams))))))

(define zero-crossings
  (general-stream-map sign-change-detector
                      sense-data
                      (cons-stream 0 sense-data)))

(define sense-data ones) ;that's just a placeholder


;Exercise 3.75
(define (bad-make-zero-crossings input last-value)
  (let ((avpt (/ (+ (stream-car input) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (bad-make-zero-crossings (stream-cdr input) avpt))))

;that's a wrong approach to averaging the values of the stream
;when the consecutive values are of different signs, we might miss the zero point
;e.g. input ...1 0.5 -0.1 -2... would lead to making comparisons between
;0.75 and 1, 0.2 and 0.5, -1.05 and -0.1
;and zero-crossing would disappear

;we shouldn't be comparing last value and the average of current and last value
;we should be comparing the average of the current and last value with the average of the current and next value
;e.g. at the point when input is 0.5: compare 0.75 and 0.2
;at the point when iput is -0.1: compare 0.2 and -1.05
;and we catch the zero-crossing where it occurs

(define (smoothed-make-zero-crossings input last-value last-average)
  (let ((avpt (/ (+ (stream-car input) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-average)
                 (smoothed-make-zero-crossings (stream-cdr input) (stream-car input) avpt))))

;Exercise 3.76
;What if we want to use a different method for smoothing?
;it's better to make things modular
(define (smooth s)
  (let ((averaged-term (/ (* (stream-car s) (stream-car (stream-cdr s))) 2)))
    (cons-stream averaged-term (smooth (stream-cdr s)))))

(define (better-make-zero-crossings input smoothing)
  (let ((smoothed-input (smoothing input)))
    (let ((e1 (stream-car smoothed-input))
          (e2 (stream-car (stream-cdr smoothed-input))))
      (cons-stream (sign-change-detector e1 e2)
                   (better-make-zero-crossings (stream-cdr input) smoothing)))))

;or
(define (better-make-zero-crossings2 input smoothing)
  (let ((smoothed-input (smoothing input)))
    (general-stream-map sign-change-detector smoothed-input (stream-cdr smoothed-input))))



