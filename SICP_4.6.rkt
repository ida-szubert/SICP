#lang racket

;Exercise 4.6

;let expressions are syntactic sugar for lambda

;(let ((v1 exp1)
;      (v2 exp2)
;      (...))
;  (body))

;is equivalent to

;((lambda (v1 v2 ...) (body))
; exp1
; exp2
; ...)


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

(define x 5)
(define y 2)
(define (square x) (* x x))

(let->combination '(let ((a (+ 1 (* x y)))
                        (b (- 1 y)))
                    (+ (* x (square a))
                       (* y b)
                       (* a b))))
;((lambda (a b) (+ (* x (square a)) (* y b) (* a b)))
; (+ 1 (* x y)) (- 1 y)))
