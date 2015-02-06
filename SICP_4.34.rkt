#lang racket

;Exercise 4.34
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;how should lazy lists be printed out?
;well, probably we want all the elements of a list evaluated before printing it out
;thunks are not convenient to read, and besides, what would be the point?
;for infinite lists, it seems reasonable to print an initial fragment of the list, e.g. 10 elements
;after all, if you need a specific element or fragment futher along you can find it otherwise than by printing a list
;infinite streams in chapter 3 just kept evaluating, you had t manually stop evaluation to inspect the output
;and it was a bit bothersome

;rigth now driver-loop prints out lists as procedures
;(compound-procedure (m) ((m x y)) <procedure-env>)
;which they are of course, but that's an implementation detail
;if I ask to see a list I want to see the sequence of its elements

;but I don't want to force evaluation of all compund procedures
;I just care about lists
;so, cons should produce tagged procedures is indistinguishable from other procedures
;it gets evaluated when driver-loop calculates output
;and the value of a lambda expression is a compound procedure

(define (cons x y) ('cons (lambda (m) (m x y))))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

(define (cons? exp) (tagged-list? exp 'cons))

(define (eval-cons exp env)
  (let ((elements (cdar (cddadr exp))))
    (list (eval (car elements) env) (eval (cadr elements) env))))

;nah, but the cons procedure should not be evaluated in that way. That's only good for printing, but not for anything else that can be done with lists
;cons expressions need to be evaluated to tagged compound procedures
;which behave differently only for the purpose of print, and for the purpose of apply they'll be like normal compound procedures
(define (eval-cons exp env)
  (cons 'cons (eval (cdr exp) env)))

(define (compound-procedure? p)
  (if (tagged-list? p 'cons)
      (tagged-list? (cdr p) 'procedure)
      (tagged-list? p 'procedure)))
;or, maybe a better idea is to modify apply

(define (lazy-apply procedure arguments env)
  (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure (list-of-arg-values arguments env)))
        ((cons? procedure) (lazy-apply (cdr procedure) arguments env))
        ((compound-procedure? procedure) (eval-sequence (procedure-body procedure) (extend-environment (procedure-parameters procedure)
                                                                                                      (list-of-delayed-args arguments env)
                                                                                                      (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))
;and compound-procedure? stays unchanged

(define (user-print object)
  (cond ((tagged-list? object 'cons) (let ((elements (cdar (cddadr exp))))
                                       (list (eval (car elements) env) (eval (cadr elements) env))))
        ((compound-procedure? object) (display
                                       (list 'compound-procedure
                                             (procedure-parameters object)
                                             (procedure-body object)
                                             '<procedure-env>)))
        (else (display object))))




; *** AGAIN ***
;when seeing a cons expression, eval evaluates it as if it was an application, ang gives it a tag 'cons
;(lazy-apply (actual-value cons env) ('a (cons 'b (cons 'c '()))) env)
;(force-it (eval cons env))
;(lookup-variable-value cons env)
;(lambda (x y) (lambda (m) (m x y)))
;(lazy-apply (lambda (x y) (lambda (m) (m x y))) ('a (cons 'b (cons 'c '()))) env)
;env gets extended, x is bound to 'a and y is bound to '(cons 'b (cons 'c '()))
;the result of lazy-apply is (actual-value (lambda (m) (m 'a (cons 'b (cons 'c '())))) env)
;and we didn't evaluate y
;(procedure (m) (m 'a (cons 'b (cons 'c '()))) env)

;(cons procedure (m) (m 'a (cons 'b (cons 'c '()))) env) is the object user-print gets
;apply proc-car to (cdr object)
;take the result and cons it onto user-print of the value of the (proc-cdr (cdr exp))
;and make a list to display by consing (i.e. underling scheme consing) the value onto the result of forced evaluation of the rest of the expression
(cons (car (cdr exp))
      (user-print (actual-value (cdr (cdr exp)) the-global-environment)))

(define (user-print object)
  (cond ((tagged-list? object 'cons) (cons (proc-car (cdr object))
                                           (user-print (actual-value (proc-cdr (cdr object)) the-global-environment))))
        ((compound-procedure? object) (display
                                       (list 'compound-procedure
                                             (procedure-parameters object)
                                             (procedure-body object)
                                             '<procedure-env>)))
        (else (display object))))

;there's a problem with (cdr exp)
;apparently the object passed to user-print is not the expression (cons procedure (m) (m 'a (cons 'b (cons 'c '()))) <env>)
;it's a procedure object
;but I don't know the difference
;oh, ok, there is no difference in the evaluator, but there is a difference in the underlying scheme
;still, don't know what to do with it

;it seems we have to intervene before the expression becomes a procedure
;so we don't want the expression to be evaluated as a procedure after all