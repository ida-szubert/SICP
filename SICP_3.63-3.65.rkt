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

(define (square x) (* x x))

;Iterations as stream processes

;square root
;let's generate an infinite stream of successively improving guesses
(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)

(stream-ref (sqrt-stream 2) 0)
(stream-ref (sqrt-stream 2) 1)
(stream-ref (sqrt-stream 2) 2)
(stream-ref (sqrt-stream 2) 3)
(stream-ref (sqrt-stream 2) 4)
(stream-ref (sqrt-stream 2) 5)

;pi
;pi/4 = 1 - 1/3 + 1/5 - 1/7
(define (pi-summands n)
  (cons-stream (/ 1.0 n) (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(stream-ref pi-stream 0);4.0
(stream-ref pi-stream 1);2.666666666666667
(stream-ref pi-stream 2);3.466666666666667
(stream-ref pi-stream 3);2.8952380952380956
(stream-ref pi-stream 4);3.3396825396825403
(stream-ref pi-stream 5);2.9760461760461765
(stream-ref pi-stream 6);3.2837384837384844

;it doesn't really converge fast
;but we can accelerate it using Euler's transform
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2  s1) s2)))
                 (euler-transform (stream-cdr s)))))

(stream-ref (euler-transform pi-stream) 0);3.166666666666667
(stream-ref (euler-transform pi-stream) 1);3.1333333333333337
(stream-ref (euler-transform pi-stream) 2);3.1452380952380956
(stream-ref (euler-transform pi-stream) 3);3.13968253968254
(stream-ref (euler-transform pi-stream) 4);3.1427128427128435
;that's much better
;but we can use euler-transform again, and it will be even faster

;better yet,
;create a stream of steams (tableau), in which each stream is the transformation of the previous one
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

;then, we make a sequence by taking the first element from each of the sequences in the tableau
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

;(display-stream (accelerated-sequence euler-transform pi-stream))
;4.0
;3.166666666666667
;3.142105263157895
;3.141599357319005
;3.1415927140337785
;3.1415926539752927
;3.1415926535911765
;3.141592653589778
;3.1415926535897953
;3.141592653589795
;...

;Exercise 3.63
;Why mot define sqrt-stream like that:
(define (sqrt-stream2 x)
  (cons-stream 1.0 (stream-map 
                    (lambda (guess) (sqrt-improve guess x))
                    (sqrt-stream2 x))))

;because we end up generating (sqrt-stream x) separately in the stream-map procedure?
;whereas guesses just uses what has already been generated?

;Exercise 3.64
(define (stream-limit s tolerance)
  (let ((first-element (stream-car s))
        (second-element (stream-car (stream-cdr s))))
    (if (< (abs (- first-element second-element)) tolerance)
        second-element
        (stream-limit (stream-cdr s) tolerance))))
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.001)
;1.4142135623746899

;Exercise 3.65
(define (ln2-summands n)
    (cons-stream (/ 1.0 n) (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

;(display-stream ln2-stream)
;I've inspected 440 elements, and it's still not converged
;0.694611306264704

(define better (euler-transform ln2-stream))
;(display-stream better)
;the 3rd elements is an approximation of the same level that the 440th above
;the 14th element is adequate to the 4th decimal place (0.6931748806748808)
;the 182nd element is adequate to the 7th decimal place

(define best (accelerated-sequence euler-transform ln2-stream))
;(display-stream best)
;converges after to 10th element 
;1.0
;0.7
;0.6932773109243697
;0.6931488693329254
;0.6931471960735491
;0.6931471806635636
;0.6931471805604039
;0.6931471805599445
;0.6931471805599427
;0.6931471805599454







