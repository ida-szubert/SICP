#lang planet neil/sicp
;(#%require (only r5rs set-car! set-cdr!))
;(require scheme/mpair)

;Domain Languages
;as we confront increasingly complex problems, we'll find that no fixed language is sufficient
;we can formulate new languages and implement them by constructing evaluators (interpreters)
;an interpreter for a language is a procedure that, when applied to an expression, performs the action required to evaluate that expression

;"The evaluator, which determines the meaning of expressions in a programming language, is just another program"


;LISP EVALUATOR
;matacircular evaluator- one written in the same language that it evaluates
;the evaluator will be a formulation of the environment model of evaluation
;in this model evaluating a combination means evaluting subexpresssions and applying the value of the oeprator to the values of the operands
;to apply a procedure to a set of arguments, construct new environment in which the formal parameters of the procedure are bound to the arguments
;evaluate the body of the procedure in this new environment

;evaluation - expressions to be evaluated in environments are reduced to procedures to be applied to arguments
;which in turn are reduced to new expressions to be evaluated in new environments, etc
;untill we get to symbols, whose values are stored in the environment, and primitive procedures
;eval (evaluate expressions)
;apply (apply procedures)

;primitives are dealt with
;evaluator provides the means of combination and the means of abstraction
;nested expressions- e.g + expects to get numbers, not expressions. Evaluator makes sure nested expression is reduced to a number before passing it to +
;variables- evaluator keeps track of variables and obtains their values before invoking primitive procedures
;compound procedures- keeping track of definitions, knowing how to use them in evaluating expressions, mechanism for procedures accepting arguments
;special forms

(define apply-in-underlying-scheme apply)

;EVAL
;takes as arguments an expression and an environment
;is a case analysis of the syntactic type of the expression
;what type an expression has hould be determined abstractly, with no commitment to particular implementations (so using predicates)
;i.e. asking assignment? intead of checking if the operator is set!
;there are also abstract selectors for parts of the expressions
;self-evaluatin expressions: eval returns the expression itself
;it can also look up the value of variables in the environment
;quoted expressions: eval returns what was quoted
;definitions/assignments: eval is called to compute the value of the varaible, and the environment is modified
;if: special processing, only one clause evaluated
;lambda: transformed into a procedure by packing the parameters and body with the environment of evaluation
;begin: evaluating a sequence of expressions in order
;cond: transformed into a nested if
;combinations: evaluate the operator and operand parts

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) (metacircular-apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

;APPLY
;takes as arguments a procedure and a list of arguments
;there's apply-primitive-procedures
;compound procedures are applied by sequentially evaluating expressions that make up the body of the procedure
;evaluation environment is extended by taking the base environment and binding the parameters of the procedure to its arguments

(define (metacircular-apply procedure arguments)
  (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure) (eval-sequence (procedure-body procedure)
                                                        (extend-environment (procedure-parameters procedure)
                                                                            arguments
                                                                            (procedure-environment procedure))))
        (else (error "Unknown procedure type: METACIRCULAR-APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)


(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

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

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;Testing of predicates
(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))
;i.e everything is true, axcept for the <false> object

;Representing procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;we assume the (apply-primitive-procedure proc args) and (primitive-procedure? proc) are available

;Environments
;an environment is a sequence (list) of frames, where each frame is a table of bindings, associating variables and values
;a frame is a pair of lists- one with variables, the other with values
;we need to be able to look up variable values,
;extnd the environment,
;define a variable,
;and set values of existing variables

(define the-empty-environment '())
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals)))) 

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;Running the evaluator as a program
;to run it, we need a mechanism that calls on the underlying Lisp system to model the application of primitive procedures

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
;global environment contains bindings for primitive procedures and for the symbols true and false
;eval and apply only need to recognize primitive procedure names (primitive?) and know how to apply the objects (apply-primitive)

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))
;a primitive procedure represented by a tagged list, containing a procedure in the underlying Lisp that implements this primitive

#|(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures)) |#
        
;how to apply a primitive procedure? Apply the implementation procedure to the arguments, using the underlying Lisp
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc)
                              args))
;why not simply "apply" that we have already used earlier in normal programs (i.e. not the interpreter)?
;because of confilic that would ensue if the name was the same (???)
;so, we only change the name of the primitive apply to apply-in-underlying-scheme
;and this needs to be done before the definition of the evaluator's apply


;read-eval-print loop of the underlying Lisp will be modeled by driver loop
(define input-prompt "::: M-eval input:")
(define output-prompt "::: M-eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
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

the-global-environment

;(define l (list 'car car))
;l
;(lookup-variable-value 'car the-global-environment)
;(metacircular-apply (lookup-variable-value 'car the-global-environment)
;(list-of-values '('(4 5 6)) the-global-environment))


;Using racket, I have problems with assignment
;no matter what, set-car! and set-cdr! don't want to work

;Using planet neil/sicp, I have problems with procedure representation
;when I'm trying to apply a primitive procedure, apply doesn't recognize the procedure as primitive
;(apply '(primitive #<procedure:mcar>) args) results in the following error:
;Unknown procedure type: APPLY #<procedure:mcar>
;it's in fact (apply-primitive-procedure '(primitive #<procedure:mcar>) args)
;and that's (apply #<procedure:mcar> args)
;BECAUSE I've said that apply-in-underlying-scheme is anothe rname for apply
;but it's not!
;(I mean, it is, but for a different apply that the one used by the implementer, namely for the primitive procedure apply, used first in chapter 2)
;SICP says that we need to rename the primitive before the definition of the evaluator's apply
;but this doesn't work
;apply: undefined; cannot reference and identifier before its definition
;maybe the the evaluator's apply should be renamed instead, e.g. to metacircular-apply
;and it works
  