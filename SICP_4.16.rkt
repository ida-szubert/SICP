#lang racket

;Exercise 4.16
;a.
#|
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (if (eq? (car vals) '*unassigned*)
                                      (error "An attempt to use a variable without assigned value:" var)
                                      (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))
|#

;b.
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (lambda-internal-defines body)
  (if (null? body)
      '()
      (if (definition? (car body))
          (cons (car body) (lambda-internal-defines (cdr body)))
          (lambda-internal-defines (cdr body)))))

(define (lambda-action-clauses body)
  (if (null? body)
      '()
      (if (definition? (car body))
          (lambda-action-clauses (cdr body))
          (cons (car body) (lambda-action-clauses (cdr body))))))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
  
(define (extract-variables definition-list)
  (if (null? definition-list)
      '()
      (cons (definition-variable (car definition-list))
            (extract-variables (cdr definition-list)))))

(define (extract-values definition-list)
  (if (null? definition-list)
      '()
      (cons (definition-value (car definition-list))
            (extract-values (cdr definition-list))))) 
              
(define (make-let variables values body)
  (define (make-bindings-list variables values)
    (if (eq? (length variables) (length values))
        (if (null? variables)
            '()
            (cons (cons (car variables) (cons (car values) '()))
                  (make-bindings-list (cdr variables) (cdr values))))
        (error "Unequal amount of variables and values to be bound")))
  (cons 'let (cons (make-bindings-list variables values)
                   body)))
       
(define (make-assignment variables values)
  (define (assignment variable value)
    (list 'set! variable value))
  (if (eq? (length variables) (length values))
      (if (null? variables)
          '()
          (cons (assignment (car variables) (car values))
                (make-assignment (cdr variables) (cdr values))))
      (error "Unequal amount of variables and values to be assigned")))

(define (*unassigned*-list x)
  (define (help counter)
    (if (eq? counter x)
        '()
        (cons '*unassigned* (help (+ counter 1)))))
  (help 0))

;(make-assignment '(u v) '((+ a 2) (+ b 3)))
;(*unassigned*-list 5)

(define (scan-out-defines body)
  (let ((variables (extract-variables (lambda-internal-defines body)))
        (values (extract-values (lambda-internal-defines body))))
    (make-let variables
              (*unassigned*-list (length variables))
              (append (make-assignment variables values)
                      (lambda-action-clauses body)))))


(define example-body
  '((define u (+ a 2))
    (define v (* b 3))
    (/ u v)))

(scan-out-defines example-body)

;c
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(define (procedure-body p) (caddr p))
;Application of a procedure involves extracting the procedure body, and applying it to the arguments
;so probably it's not a great idea to use scan-out-defines in procedure-body rather than in make-procedure
;because then the scanning out would need to be performed every time the procedure is applied
;when scanning out happens within make-procedure, it'll only need to happen once
            

                        
              