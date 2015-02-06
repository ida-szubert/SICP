#lang racket

;Exercise 4.23
;analyze-sequence seems very complicated

(define (analyze-sequence1 exps)
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
;we take a sequence of expressions, and change it into sequence of execution procedures
;then we take the first procedure, and if there are no procedures left, we return it as the result of analyze-sequence
;but if we're not looking at the last procedure, we sth slightly strange
;we take two first procedures and produce a functon which executes these two procedures sequentially
;this function is now passed onto loop as the first-proc
;if there were only two expressions in the sequence to start with, analyze-sequence will return this new function
;in the end we get a function with one argument (environment) which applies every execution process in the sequence to the given environment

;Why shouldn't analyze-sequence be more similar to eval-sequence?
(define (analyze-sequence2 exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs env) (execute-sequence (cdr procs env))))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        (lambda (env) (execute-sequence procs env)))))
;the problem with this definition is that although individual expressions are analyzed, it never produces an analysis of the whole sequence



;SEQUENCE WITH ONE EXPRESSION
;analyze-sequence1 calls (loop proc '())
;loop returns proc
;done


;analyze-sequence2 creates a function
;(lambda (env) (execute-sequence procs env))
;thus (lambda (env) (proc env)) is returned
;done
;well, this seems superfluous, since once we've changed the sequence of expressions to a sequence of processes
;each process is a function of env
;and here we are wrapping it in another function of env
;it doesn't hurt, but why do it?


;SEQUENCE WITH TWO EXPRESSIONS
;analyze-sequence1 calls (loop proc1 (proc2))
;loop calls (sequentially proc1 proc2)
;it returns (lambda (env) (proc1 env) (proc2 env))
;loop calls itself (loop (lambda (env) (proc1 env) (proc2 env)) '())
;loop returns (lambda (env) (proc1 env) (proc2 env))
;done

;analyze-sequence2 creates a function
;(lambda (env) (execute-sequence (proc1 proc2) env))
;exectute-sequence creates an applicatio (proc1 env), which is executed
;and execute-sequence calls itself
;(execute-sequence (proc2) env)
;exectute-sequence creates an applicatio (proc2 env), which is executed
;done

;analyze-sequence2 analyzes expressions in turn, but it does not analyze the sequence

