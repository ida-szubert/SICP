#lang racket

;Exercise 4.1
;we don't know if the metacircular evaluator evaluates from left to right or the other way round
;the evaluation order depends on the underlying Lisp

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (left-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval-first-operand exps) env))
        (cons first-value
              (list-of-values (rest-operands exps) env)))))

(define (right-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-of-values (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest-of-values))))