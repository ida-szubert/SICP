#lang racket

;Exercise 4.8
(define (let->combination exp)
  (let ((new-lambda (make-lambda (variable-list (let-bindings exp))
                                       (cons (let-body exp) '())))
        (bound-expressions (expression-list (let-bindings exp))))
    (if (named-let? exp)
        (make-application (make-named-lambda (cadr exp) new-lambda)
                          bound-expressions)
        (make-application new-lambda
                          bound-expressions))))



(define (variable-list bindings)
  (if (null? bindings)
      '()
      (cons (caar bindings) (variable-list (cdr bindings)))))
  
(define (expression-list bindings)
  (if (null? bindings)
      '()
      (cons (cadar bindings) (expression-list (cdr bindings)))))
  
(define (named-let? exp)
  (symbol? (cadr exp)))

(define (let-bindings exp) (if (named-let? exp)
                           (caddr exp)
                           (cadr exp)))
  
(define (let-body exp)  (if (named-let? exp)
                        (cadddr exp)
                        (caddr exp)))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-named-lambda name lambda-exp)
  (cons 'define (cons (cons name (cadr lambda-exp)) (cddr lambda-exp))))

(define (make-application expression arguments)
  (cons expression (cons arguments '())))



(let->combination '(let fib-iter ((a 1)
                                  (b 0)
                                  (count 5))
                     (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))

(let->combination '(let ((a 1)
                         (b 0)
                         (count 5))
                     (if (= count 0) b (+ a b))))