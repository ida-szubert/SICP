#lang racket
;nested mappings
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;given a positive integer n fing all ordered pairs such that 1<= j < i <= n
;and that i+j is a prime

;generate the sequence of all ordered pairs of positive integers in the range 1 to n
;filter to find only tgose whose sum is a prime
;how to generate the sequence of pairs?
;for each i <= n, enumerate all j < i; for each enumerated j, create a pair i,j
;so, we'll map along a sequence (enumerate-interval 1 n)
;for each element i we'll (enumerate interval 1 (- i 1))
;for each element j of that sequence we'll create the pair (list i j)
;we'll accumulate the pairs generated for all i-s with append

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;(accumulate append '() (map (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
 ;                           (enumerate-interval (1 n)))
            
;the combination of mapping and appending is very common - enought to want to have a separate procedure for doing that
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;(flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
;         (enumerate-interval (1 n)))

;now onto filtering the right pairs
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))
(define (square x) (* x x))

;to generate the sequence of results we need to map over the sequence of filtered pairs
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;the complete procedure:
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(prime-sum-pairs 6)


;Another problem- how to find permutations of a set?
;for each item x in S, recursively generate the sequence of permutations of S-x, and adjoin x to the front of each one
(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

;Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))

(define (prime-sum-pairs2 n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 6))
(map (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 6))
   
;Exercise 2.41
;find all ordered triples of distinct positive integers i j k <= n
;i j k must sum to a given integer s

;enumerate integers from 1 to n
;for each element i from this list, enumerate from 1 to i-1
;for each element j from this list, enumerate from 1 to j-1

(define (unique-triples n)
  (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list i j k))
                                         (enumerate-interval 1 (- j 1))))
                        (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))

(unique-triples 6)

(define (s-sum-triples n s)
  (filter (lambda (t)
            (= (+ (car t) (cadr t) (caddr t)) s))
          (unique-triples n)))

(s-sum-triples 6 12)

;Exercise 2.42
;we want to place a queen in the kth column
;in the previous step all the ways to place k-1 queens in k-1 colums have been generated
;each one of those ways can be extended in 8 ways, by placing the kth queen in any of the 8 rows of the kth column
;once we have all the possible patterns for the placing of 8 queens, we filter to get only the ones were all the queens are safe
;queens - a procedure returning a sequence of all solutions to the problem of placing n queens on an n*n board
;queens-cols - returns the sequence of all ways to place queens in the first k columns

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter 
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (position row col)
  (cons row col))
(define (column position)
  (cdr position))
(define (row position)
  (car position))

(define empty-board ('()))
;rest-of-queens is a set of all possible positions of k-1 queens
(define adjoin-position (row k sequence)
  (map (cons (position row k) sequence)))
;given the set of possible positions and the column number, we decide whether a position whose colums = k is a safe one with respect to all other positions
(define (safe? k positions)
  (
        
        

  
            