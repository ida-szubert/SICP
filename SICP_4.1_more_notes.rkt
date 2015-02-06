#lang planet neil/sicp

;Important things:
;an evaluator is a universal machine
;it mimics other machines when these are described as Lisp programs

;an evaluator acts as a bridge between the data objects manipulated by the language and the language itself
;when an evaluator is running, the user types in what's from his perspective a string in the language
;for the evaluator, it's a piece of data to be manipulated acording to the rules
;i.e. user's programs are evaluator's data

;why not make eval available for use in programs, so that users can explicitly evaluate a data object as a Lisp expression
;this primitive eval procedure takes as arguments an expression and an environment
;these are actuall environments, not the sample structures build in 4.1.1
;actual environments cannot be manipulated by the users as lists


;Internal definitions:
;evaluating them is tricky
;in principle they shoud be evaluated simultaneously
;so that any interal definition can refer to any other, regardeless of the order in which they are written
;but there is no good way to implement this rule
;things get particularily tricky with mutually recursive definitions


;Syntactic analysis vs execution
;It's inefficient if the evaluator interleaves syntactic analysis of expressions with their execution
;if a program is executed many times, the syntax is analyzed many times
;it would be way better to perform syntactic analysis only once
;instead of eval, which takes the expression and the environment as its arguments
;we'll have analyze taking only the expression and returning an execution procedure
;and eval working with the output of analyze and the environment

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((quoted? exp) (analyze-quoted exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (tagged-list? exp tag)
  (eq? (car exp) tag))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (if? exp)
  (tagged-list? exp 'if))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (let? exp)
  (tagged-list? exp 'let))

(define (application? exp)
  (pair? exp))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


;NEW ANALYZE FUNCTIONS
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty squence: ANALYZE")
        (loop (car procs) (cdr procs)))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env)) aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc) (apply-primitive-procedure proc args))
        ((compound-procedure? proc) ((procedure-body proc) (extend-environment
                                                            (procedure-parameters proc)
                                                            args
                                                            (procedure-environment proc))))
        (else (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))


;OLD EVALUATOR FUNCTIONS
(define (begin-actions exp) (cdr exp))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

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

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (make-begin seq) (cons 'begin seq))

(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

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

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc)
         args))

(define (primitive-implementation proc) (cadr proc))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define the-empty-environment '())
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (cdr frame))))