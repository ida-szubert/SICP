#lang planet neil/sicp
(define count-change
  (lambda (amount)
    (cc amount 5)))

(define cc
  (lambda (amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else (+ (cc amount (except-first-denomination coin-values))
                   (cc (- amount (first-denomination coin-values)) coin-values))))))

(define first-denomination
  (lambda (l)
    (car l)))

(define except-first-denomination
  (lambda (l)
    (cdr l)))

(define no-more?
  (lambda (l)
    (null? l)))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1))
(define reversed-uk-coins (list 1 2 5 10 20 50 100))
(define scrambled-uk-coins (list 5 10 2 20 50 1 100))

(first-denomination (cdr us-coins))
(except-first-denomination us-coins)

(cc 100 uk-coins)
(cc 100 reversed-uk-coins)
(cc 10 uk-coins)
(cc 10 reversed-uk-coins)
(cc 1 uk-coins)
(cc 1 reversed-uk-coins)
(cc 100 scrambled-uk-coins)

;the order of the list doesn't change the result of cc
;it shouln't, because no matter what coin do we start with, we always add up the result of cc with (cdr coins)
;and the result of cc for the (car coins)
;and the specific order in which we add these values doesn't make a difference