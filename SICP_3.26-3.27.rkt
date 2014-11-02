#lang planet neil/sicp

;Exercise 3.26
;serching a table involves scanning a list of records
;for large tables it mey be efficient to structure the table differently than as an unordered set of records
;if keys can be organized, e.g. numerically or alphabetically, records can be organized using a binary tree

;so, each level of the table is like a binary tree
;let's say there are tags A B C
;and subtable a has tags 101 102 103 within
;and subtable i has tags a b
;a, b, 102, 103 are sets of records
;these can be ordered by key
;a can be ordered by key as well
;whether you want to do it probably depends on the number of subtrees

;if it's only records that are ordered as binary trees, then we would need to change assoc
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

;TODO

;Exercise 3.27
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))
;that's not a great algorithm, because we end up callculating th same stuff over and over again
;it would be much better if we could remember intermediate results

(define memo-fib
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else (+ (memo-fib (- n 1))
                    (memo-fib (- n 2))))))))
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

;drawing it out is crazy
;this wouldn't really work if we defined memo-fib as (memoize fib)
;what would happen is that every time a result was not found in the table, we would compute it from scratch
;even if there were lower results that could be used in the process
;the difference lies in memo-fib's else clause, which ensures that even if we need to calculate a result,
;we will use available intermediate results in the process


