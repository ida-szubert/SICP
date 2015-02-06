#lang racket

;Exercise 4.9

(define x 0)
(define (square x) (* x x))


;WHILE

;iI'd like to have expression of the folowing shape
;(while (< x 5)
;       (display (square x))
;       (newline)
;       (set! x (+ x 0.5)))

;which would be equivalent to
(define (foo a)
  (display (square x))
  (newline)
  (set! x (+ x 0.5))
  (if (< x 5)
      (foo a)
      'done))

(foo 'go)
                
;so, I'm going to change a while expression into a define expression
(define (while->named-lambda exp)
  (let ((if-exp (make-if (while-condition exp)
                         '(foo a)
                         ''done)))
    (let ((body (append (while-actions exp) (list if-exp))))
      (make-named-lambda 'foo
                         (make-lambda '(a) body)))))
        

(define (while-condition exp) (cadr exp))

(define (while-actions exp) (cddr exp))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (make-lambda parameters body)
  (if (pair? (car body))
      (cons 'lambda (cons parameters body)) ;this deals with mbody consisting of multiple actions
      (list 'lambda parameters body)))

(define (make-named-lambda name lambda-exp)
  (cons 'define (cons (cons name (cadr lambda-exp)) (cddr lambda-exp))))


(while->named-lambda '(while (< x 5)
       (display (square x))
       (newline)
       (set! x (+ x 0.5))))