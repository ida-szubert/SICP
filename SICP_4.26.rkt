#lang racket

;Exercise 4.26
;one could define unless as a special form, so that it would work with applicative-order evaluation
;much like if, only that the consequent and alternative clauses would be swapped

;but there's a difference between special forms and procedures
;special forms cannot be manipulated in all the ways procedures can
;e.g. you cannot map if over a list of tripples
;and it might be useful to map unless over e.g. three streams, so that unless (car stream-1) is #f, you take (car stream-2), and (car stream-3) otherwise

;AS DERIVED EXPRESSION
(define (unless? exp) (tagged-list? exp 'unless))

(define (unless-condition exp) (cadr exp))
(define (unless-usual-value exp) (caddr exp))
(define (unless-exception-value exp) (cadddr exp))

(define (eval-unless exp env)
  (eval (unless->if exp) env))

(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-exception-value exp)
           (unless-usual-value exp)))

