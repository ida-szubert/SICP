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

;Infinite streams of pairs
;let's say we want to write prime-sum-pairs which produces the stream of pairs of all integers (i, j) with i<=j such that i+j is prime
;(int-pairs will be the sequence of all pairs of integers such that i<=j
;(stream-filter (lambda (pair) (prime? (+ (car pair) (cadr pair)))) int-pairs)

;suppose we have two streams, S and T
;and we pair each element of S with eacg element of T
;if we imagine it as a matrix, with S increasing downwards and T increasing rightwards
;then what we're looking for is in the upper-right half of the matrix
(define (pairs s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                        (pairs (stream-cdr s) (stream-cdr t)))))
;it's not obvious how to combine the last two streams
;we cannot use sth like append, since it evaluates the whole first list and incorporates the second
;but we're working with infinite lists

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))



;Exercise 3.66
;(display-stream (pairs integers integers)) 
;the pair (1 100) is approximately the 197th element
;in general,it's pairs of (1 x) interleaved with other pairs
;before (1 100) there are 99 pairs with 1 as their first element
;and 98 other pairs in between these
;for a total of 197 before (1 100)
;subsequent integers are gradually introduced as the first pair element
;1 2 1 2 1 3 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2
;1 x 1 2 1 x 1 2 1 x 1 2 1 x 1 2 1 x 1 2 1 x 1 2 1 x 1 2 1 x 1 2 1 x 1 2 1 x 1 2 1 x 1 2 1 x 1 2
;there's a pattern in the first elements 1 x 1 2
;where x is 2 3 3 4 3 4 3 5 3 4 3 5
;             x 3 x 3 x 3 x 3 x 3 x
;so, there's a pattern in what integer gets the x spot in the 1 x 1 2 pattern
;it's 3 interleaved other integers, and they also follow a pattern
;but I'm no mathematician to descibe the function for the place of a pair (i, j) in the stream


;Exercise 3.67
(define (all-pairs s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                           (interleave (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s))
                                       (all-pairs (stream-cdr s) (stream-cdr t))))))
;The exercise says that we need to mix in an additional stream
;but why not just change the last stream from pairs, so that it takes t rather that (stream-cdr t)?
(define (all-pairs2 s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                           (all-pairs2 (stream-cdr s) t))))

;(display-stream (all-pairs integers integers))
;both versions work, although obviusly the pairs are generated in different order


;Exercise 3.68
;Why separate the first pair in the definition of pairs?
(define (new-pairs s t)
  (interleave (stream-map (lambda (x) (list (stream-car s) x)) t) ;this accounts for the whole stream with e.g. with 1 in the first place 
              (new-pairs (stream-cdr s) (stream-cdr t))))

;(display-stream (new-pairs integers integers))
;it doesn't evaluate
;interleave is an ordinary function, so in order to evaluate it, we need to eavluate (new-pairs (stream-cdr s) (stream-cdr t))
;so, each call to new-pair causes another call to new-pair, and the process never stops
;in the first implementation there's cons-stream, so that each time pairs is called, sth actually happens- a pair is consed onto the stream

;Exercise 3.69
(define (triples s t u)
  (cons-stream (list (stream-car s) (stream-car t) (stream-car u)) ;this produces (s0 t0 u0)
               (interleave 
                (interleave (stream-map (lambda (x) (list (stream-car s) (stream-car t) x)) (stream-cdr u));this produces all triples (s0 t0 u)
                            (stream-map (lambda (x) (cons (stream-car s) x)) (pairs (stream-cdr t) (stream-cdr u))))
                (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))));this produces all (sn tn u)
                               
(define pythagorean-triples
  (stream-filter (lambda (triple) (= (+ (square (car triple)) (square (cadr triple))) (square (caddr triple))))
                 (triples integers integers integers)))

(stream-ref pythagorean-triples 0)


;Exercise 2.70
;what if we wanted the pairs to appear in a particular order?
;we might create a weighting function
;and use it in the process of merging streams, so that less wighty items come first

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define (weighted-merge weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car)) (cons-stream s1car (weighted-merge weight (stream-cdr s1) s2)))
                 ((> (weight s1car) (weight s2car)) (cons-stream s2car (weighted-merge weight s1 (stream-cdr s2))))
                 (else (cons-stream s2car (weighted-merge weight s1 (stream-cdr s2)))))))))

(define (weighted-pairs weight s t)
  (cons-stream (list (stream-car s) (stream-car t))
               (weighted-merge weight 
                               (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                               (weighted-pairs weight (stream-cdr s) (stream-cdr t)))))

;a
(define (pair-weight pair)
  (+ (car pair) (cadr pair)))

(define stream (weighted-pairs pair-weight integers integers))
;(display-stream stream)
;hmm, now I get a stream of all the pairs starting with 1
;ok, so the mistake was that in weighted-merge, when the weights of s1car and s2car are equal
;we cannot just cons whichever onto the final stream and forget about the other
;because pairs with equal sum can have different elements
;so we cons s1car, and recur weighted-merge not with (cdr s2), but with s1
;of course, the order will be different depending on whether you cons s1car or s2car
;both resulting streams will be ordered according to the weight of pairs, but differently
;s1car first   s2car first
;(1 1)         (1 1)
;(1 2)         (1 2)
;(1 3)         (2 2)
;(2 2)         (1 3)
;(1 4)         (2 3)
;(2 3)         (1 4)
;(1 5)         (3 3)
;(2 4)         (2 4)
;(3 3)         (1 5)
;(1 6)         (3 4)
;(2 5)         (2 5)
;(3 4)         (1 6)
;(1 7)         (4 4)
;(2 6)         (3 5)
;(3 5)         (2 6)
;(4 4)         (1 7)
;(1 8)         (4 5)
;(2 7)         (3 6)
;(3 6)         (2 7)
;(4 5)         (1 8)
;(1 9)         (5 5)

;b.
(define (strange-weight pair)
  (+ (* (car pair) 2) (* (cadr pair) 3) (* (car pair) (cadr pair) 5)))

(define elements 
  (stream-filter (lambda (x) (not (or (divisible? x 2) (divisible? x 3) (divisible? x 5))))
                 integers))

(define stream2
  (weighted-pairs strange-weight elements elements))

;(display-stream stream2)
;(1 1)
;(1 7)
;(1 11)
;(1 13)
;(1 17)
;(1 19)
;(1 23)
;(1 29)
;(1 31)
;(7 7)
;(1 37)
;(1 41)
;(1 43)
;(1 47)
;(1 49)
;(1 53)

;Exercise 3.71
(define (cube x) (* x x x))
(define (cube-weight pair)
  (+ (cube (car pair)) (cube (cadr pair))))

(define cube-ordered-pairs
  (weighted-pairs cube-weight integers integers))

(define (compare-consecutive s weight)
  (let ((pair1 (stream-car s))
        (pair2 (stream-car (stream-cdr s))))
    (if (= (weight pair1) (weight pair2))
        (cons-stream (list pair1 pair2)
                     (compare-consecutive (stream-cdr s) weight))      
        (compare-consecutive (stream-cdr s) weight))))

(define ramanujan-pairs
  (compare-consecutive cube-ordered-pairs cube-weight))

(define ramanujan-numbers (stream-map (lambda (x) (cube-weight (car x))) ramanujan-pairs))

;(stream-ref ramanujan-numbers 0);1729
;(stream-ref ramanujan-numbers 1);4104
;(stream-ref ramanujan-numbers 2);13832
;(stream-ref ramanujan-numbers 3);20683
;(stream-ref ramanujan-numbers 4);32832
;(stream-ref ramanujan-numbers 5);39312

;Exercise 3.72
(define (square-weight pair)
  (+ (square (car pair)) (square (cadr pair))))

(define square-ordered-pairs
  (weighted-pairs square-weight integers integers))

(define (compare-three-consecutive s weight)
  (let ((pair1 (stream-car s))
        (pair2 (stream-car (stream-cdr s)))
        (pair3 (stream-car (stream-cdr (stream-cdr s)))))
    (if (= (weight pair1) (weight pair2) (weight pair3))
        (cons-stream (list pair1 pair2 pair3)
                     (compare-three-consecutive (stream-cdr s) weight)) ;it could be (c-t-c (stream-car (stream-cdr (stream-cdr (stream-cdr s)))))  
        (compare-three-consecutive (stream-cdr s) weight))))

(define wanted-pairs
  (compare-three-consecutive square-ordered-pairs square-weight))

(define wanted-stream
  (stream-map (lambda (x) (square-weight (car x))) wanted-pairs))
;it could be done much easier if I didn't want to have the actual pairs easily accessible for reference
;compare(-three)-consequtive could simply generate a stream of numbers rather than a stream of lists

;(stream-ref wanted-stream 0);325
;(stream-ref wanted-stream 1);425
;(stream-ref wanted-stream 2);650
;(stream-ref wanted-stream 3);725
;(stream-ref wanted-stream 4);845
;(stream-ref wanted-stream 5);850
;(stream-ref wanted-pairs 0);(10 15) (6 17) (1 18)
;(stream-ref wanted-pairs 1);(13 16) (8 19) (5 20)
;(stream-ref wanted-pairs 2);(17 19) (11 23) (5 25)
;(stream-ref wanted-pairs 3);(14 23) (10 25) (7 26)
;(stream-ref wanted-pairs 4);(19 22) (13 26) (2 29)
;(stream-ref wanted-pairs 5);(15 25) (11 27) (3 29)

;I could also generate a stream of lists (value pair1 pair2 pair3) to have all the info together
(define all-in-one
  (stream-map (lambda (x) (list (square-weight (car x)) x)) wanted-pairs))

(stream-ref all-in-one 0);(325 (10 15) (6 17) (1 18))
(stream-ref all-in-one 1);(425 (13 16) (8 19) (5 20))


