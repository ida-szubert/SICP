#lang racket

;Exercise 4.52
;if-fail
;takes two expressions as arguments
;it evaluates the first, and if evaluation fails, the second

(define (analyze-if-fail exp)
  (let ((first-exp (cadr exp))
        (second-exp (caddr exp)))
    (lambda (env succeed fail)
      (first-exp env
                 succeed
                 (lambda ()
                   (second-exp env succeed fail))))))

;Exercise 4.53
(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (permanent-set! pairs (cons p pairs))
             (amb))
           pairs))

;first expression if evaluated first
;there is more than one pair of elements from the first and second lists that produces a prime
;determining the value of p is a choice point
;(3 20) is the first one
;it gets consed onto pairs
;then evaluation fails, and is backtracked to the nearest choice point
;this time p is (3 110)
;this gets consed onto pairs
;failure and backtracking again, (8 35) gets consed onto pairs
;failure agian, there are no more choices, so evaluation of the first expression finally failed
;second expression is evaluated, and the value is ((8 35) (3 110) (3 20))


;Exercise 4.54
;require can be written as a normal function, part of a nondeterministic program
;but it could also be implemented as a special form

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (false? pred-value)
                   fail
                   (succeed 'ok fail2)))
             fail))))