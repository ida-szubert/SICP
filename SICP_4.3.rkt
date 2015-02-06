#lang racket

;Exercise 4.3
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

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) ((get 'eval 'variable) exp env))
        ((pair? exp)
         (let ((specific-form (get 'eval '(car exp))))
           (if (specific-form)
               (specific-form exp env)
               (apply (eval (operator exp) env)
                      (list-of-values (operands exp) env)))))))

;we'd also need a way to put specific eval procedures in a table

(put 'eval 'quoted text-of-quotation)
(put 'eval 'assignment eval-assignment)
(put 'eval 'definition eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda (lambda (x y) (make-procedure (lambda-parameters x)
                                       (lambda-body x)
                                       y)))
(put 'eval 'begin (lambda (x y) (eval-sequence (begin-actions x) y)))
(put 'eval 'cond (lambda (x y) (eval (cond->if x) y)))

        
        
        