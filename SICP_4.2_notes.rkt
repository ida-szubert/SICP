#lang racket

;Variations on scheme

;normal vs applicative order
;applicative-order: all arguments are evaluated when the procedure is applied
;normal-order: arguments are evaluated when their values are actually needed
;delaying evaluation until the last possible moment is called lazy evaluation

(define (try a b) (if (= a 0) 1 b))
;(try 0 (/ 1 0) would produce an error in applicative-order evaluation, but not with normal-order evaluation

;unless depends on lazy evaluation
;in applicative-order, the usual-value would be computed even if the condition holds
;and the whole point of unless might be not to compute the usual-value at all when the condition holds, e.g. because we know computing it wouldn't work or make any sense
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(unless (= b 0) (/ a b) (begin (display "exception: returning 0") 0))

;it might have been useful if cons was non-strict, so that when procedure f conses sth onto a recursive call to f
;cons could be executed before the call to f returns a value, instead of waiting around and consuming space
;also, e.g. it makes sense to check the length of a list without evaluating the list


;Lazy evaluation version of Scheme
;primitive procedures are strict
;what happens with arguments that are not evaluated? They are transformed into thunks.
;A thunk contains info required to produce the value of the argument when it will be needed, i.e. argument expression and the environment at the time of calling
;To get a value, you can force a thunk
;it might be a good idea to use memoization on the values, so that each thunk will be evaluated only once, and the expression will be stored in a table
;(lazy evaluation plus memoization is sometimes referred to as call-by-name)

((appliaction? exp) (apply (actual-value (operator exp) env)
                           (operands exp)
                           env))
;operator is evaluated, operands are passed as expressions, and additionally the environment is passed as well, because it's needed to make thunks

(define (actual-value exp env)
  (force-it (eval exp env)))

;how will apply work?
;for primitive procedures, all the arguments will be evaluated before applying the procedure
;for compound procedures, the arguments will be delayed

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure (list-of-arg-values arguments env)))
        ((compund-procedure? procedure) (eval-sequence (procedure-body procedure) (extend-environment (procedure-parameters procedure)
                                                                                                      (list-of-delayed-args arguments env)
                                                                                                      (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define input-prompt ";;; L-eval input:")
(define output-prompt ";;; L-eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;--> Problem is, neither racket not planet neil/sicp know what force-it and delay-it are; using force and delay doesn't work either <---


;Thunks

;(define (force-it obj)
;  (if (thunk? obj)
;      (actual-velue (thunk-exp obj) (thunk-env obj))
;      obj))
;that's ok if we don't use memoization

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))


;Streams as lazy lists
;in chapter 3 delay and stream-cons were special forms
;it's not perfect
;pecial forms are not first-class objects and cannot be used together with higher-order procedures
;also, streams had to be created as a spearate data object from lists, which means reimplementing many procedure for use with streams
;with lazy evaluation lists and streams can be identical
;cons needs to be non-strict
;which would mean that we need an exception from the 'primitives are strict' rule
;or cons might not be a primitive at all, if we represent pairs as procedures
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))))
(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))
(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
              (add-lists (cdr list1) (cdr list2))))))
(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))

;lazy lists are evan lazier than streams, because car as well as cdr is delayed
;and the values will be forced only when really needed- to be printed as an answer or to be used as an argument of a primitive
;also, with lazy evaluation extended to cons/car/cdr all arguments to all procedures are delayed in the same way

(define (integral integrand initial-value dt)
  (define int
    (cons initial-value (add-lists (scale-list integrand dt) int)))
  int)
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y)