#lang planet neil/sicp

;Exercise 4.30
;some side effects may never take place, because the lazy evaluator does not force the expressions in a sequence
;since all expressions of a procedure body but the last one are not used, there is no way for them to be forced

(define (new-eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (new-eval-sequence (rest-exps exps) env))))

(define (original-eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (original-eval-sequence (rest-exps exps) env))))
;a
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x)) '(57 321 88))


;This works fine with the original eval-sequence
;the interpreter prints out the numbers, and then with the output prompt returns the result 'done
;this is an application of a compound procedure
(eval-sequence ((if (null? items)
                   'done
                   (begin (proc (car items))
                          (for-each proc (cdr items)))))
               extended-env) ;where proc - (lambda (x)..., and items - (57 321 88)
;this sequence has one expression only
;if-expression gets evaluated in the extended environment
;(if-alternative gets evaluated, which is a sequence, so
(eval-sequence (((lambda (x)...) 57)
                (for-each (lambda (x)...) (321 88)))
               extended-env)
;so, the function gets applied to the 57 (that's what (car items) means in the extended environment)
;(lambda (x)...) is evaluated into a procedure, the procedure
;the procedure, whose body is a sequence, is lazy-applied to 57
;this means that we eval-sequence the body, in an environment where x is bound to 57
;then, for-each gets called again, with new arguments

;nothing would change were new-eval-sequence used
;that's because there are no thungs in signt, so actual-value would be the same as eval


;b
(define (p1 x) (set! x (cons x '(2))) x)

(define (p2 x)
  (define (p e)
    e x)
  (p (set! x (cons x '(2)))))

;ORIGINAL EVAL-SEQUENCE
; > (p1 1)
; (1 2)
; > (p2 1)
; 1

;in the evaluation environment x is bound to whatever is fed to p1
;p1 changes the binding and returns what's now bound to x

;in the evaluation environment x is bound to whatever is fed to p2
;p2 can be expected to do two things
;first, do sth with its argument
;and second, return x
;it's unclear whether we should expect to get the original x or (x 2)
;the set! expression is the argument of p and it gets delayed
;then eval-sequence tries to eval the thunk
;and for some reason it cannot, but there's no error message...
;and then the next expression in a sequence is evaluated, i.e. x, which is still bound to the argument fed to p2
;and 1 is returned

;NEW EVAL-SEQUENCE
; > (p1 1)
; (1 2)
; > (p2 1)
; (1 2)

;with the new eval-sequence the delayed set! expression is forced
;x is set to (x 2)
;and (x 2) is what p2 returns


;c.
;because, as I've said before, actual-value and eval give the same result when applied to non-thunks, and there are no thunks in the part a. example

;d.
;well, if there's more than one expression in the body of a procedure, then they are there for a reason
;it would be confusing not to evaluate them
;on the other hand, the idea of having a lazy evaluator is that we only evaluate the expressions that are needed at a given point
;and side effects are never needed, in the sense of "need" that's used here
;also, one can get side effects with the original eval-sequence, as shown by p1 and for-each examples
;so maybe not forcing expressions in a sequence is a good decision
;side-effects can be used, and the lazy-evaluation idea is preserved
;one just needs to think carefuly about how will side effects work ih their code, if one wants them at all





