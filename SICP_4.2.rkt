#lang racket

;Exercise 4.2
;reordering cond clauses in eval
;so that procedure application appears before assignment
;reasoning being that there are usually more applications than assignments, and reordering will decrease the number of clause checks

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (look-up-varaible exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

;a.
;that's not a reasonable plan
;application comes last, because otherwise we would treat all kinds of expressions as applications, including all these that are not
;(definitions, assignments, lambdas, ifs, cond, begin)
;the application? predicate only checks if the expression is a pair
;it can be so simple precisely because we've already eliminated the possiblility of a pair being anything other than application
;we could make (application? exp) the first clause
;but then the application? predicate would need to check if the expression is not of any of the 9 types listed in eval
;such a solution would not provide the planned benefits, but it would in fact increase the load

;b.
;we could change the syntax of the evaluated language
;and give procedure applications a tagword

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))