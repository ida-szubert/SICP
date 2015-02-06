#lang racket

;Exercise 4.33
;lazy car and cdr work with lists created by lazy cons, but not with lists obtained  by reading in quoted expressions
;we need to manipulated the evaluator so thatquoted lists are transformed into lazy lists when typed into the driver loop

;which means that the (quoted? exp) clause of eval needs to change
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) (lazy-apply (actual-value (operator exp) env)
                                        (operands exp)
                                        env))
        (else (error "Unknown expression type: EVAL" exp))))



;(quote (a b c)) needs to act as if it was
;(quote (cons a (cons b (cons c '()))))
;then text-of-quotation would be a list that can be manipulated


;new-cons is installed as a primitive into the evaluator as cons
;but in the code of the evaluator, cons in the original cons and new-cons is new-cons
;the quote? clause in eval stays as it was
;and text-of-quotation is now
(define (text-of-quotation exp)
  (define (iter list-to-be)
    (if (null? list-to-be)
        '()
        (new-cons (car list-to-be) (iter (cdr list-to-be)))))
  (iter (cdr exp)))


;Ok, I don't know
;one of the solutions on Scheme wiki works when checked in the lazy_evaluator file
(define (text-of-quotation expr env)
  (define (make-list exp)
    (if (null? exp)
        (list 'quote '())
        (list 'cons
              (list 'quote (car exp))
              (make-list (cdr exp)))))
  (let ((text (cadr expr))) 
    (if (pair? text) 
        (eval (make-list text) env) 
        text)))
               


