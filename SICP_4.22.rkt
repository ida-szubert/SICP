#lang racket

;Exercise 4.22

(define sample-let '(let ((a (+ 1 (* x y)))
                          (b (- 1 y)))
                      (+ (* x (square a))
                         (* y b)
                         (* a b))))

(define (let->combination exp)
  (define (variable-list bindings)
    (if (null? bindings)
        '()
        (cons (caar bindings) (variable-list (cdr bindings)))))
  (define (expression-list bindings)
    (if (null? bindings)
        '()
        (cons (cadar bindings) (expression-list (cdr bindings)))))
  (let ((bindings (cadr exp))
        (body (cddr exp)))
  (cons (make-lambda (variable-list bindings)
                     body)
        (expression-list bindings))))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

                  

(define (analyze-let exp)
  (analyze (car (let->combination exp))))

;i.e. we install into the analyze procedure the following clause:
;((let? exp) (analyze (let->combination exp)))
;and in effect we will analyze an application
  
