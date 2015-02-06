#lang racket

;Exercise 4.4

(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

;special forms:
(define (eval-and exp env)
  (if (last-clause? (first-clause exp))
      (eval (first-clause exp) env)
      (if (true? (eval (first-clause exp) env))
          (eval-and (rest-clauses exp) env)
          'false)))

(define (eval-or exp env)
  (if (null? exp)
      'false
      (if (true? (eval (first-clause exp) env))
          (eval (first-clause exp) env)
          (eval-or (rest-clauses exp) env))))


;derived expressions:
(define (eval-and exp env)
  (eval (and->if (and-clauses exp)) env))

(define (and->if clauses)
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (make-if first
                 (and->if rest)
                 'false)))

(define (and-clauses exp) (cdr exp))

(define (eval-or exp env)
  (eval (or->if (or-clauses exp)) env))

(define (or->if exp env)
  (let ((first (car clauses))
        (rest (cdr clauses)))
    (make-if first
             'true
             (or->if rest))))
                   
(define (or-clauses exp) (cdr exp))
  
  
 