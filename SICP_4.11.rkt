#lang racket

;Exercise 4.11
;a frame can be represented as a list of bindings

(define the-empty-environment '())
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))

(define (make-frame variables values)
  (if (null? variables)
      '()
      (cons (make-binding (car variables) (car values))
            (make-frame (cdr variables) (cdr values)))))

(define (make-binding var val)
  (list var val))
        
;(define (frame-variables frame) (car frame))
;(define (frame-values frame) (cdr frame))
;we can have these selectors for convenience, even though other procedures don't need them

(define (frame-variables frame)
  (if (null? frame)
      '()
      (cons (caar frame) (frame-variables (cdr frame)))))
  
(define (frame-values frame)
  (if (null? frame)
      '()
      (cons (cdar frame) (frame-values (cdr frame)))))


(define (add-binding-to-frame! var val frame)
  (set! frame (cons (make-binding var val) frame)))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals)))) 

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (let ((first (car bindings))
            (rest (cdr bindings)))
        (cond ((null? bindings) (env-loop (enclosing-environment env)))
              ((eq? var (car first)) (cadr first))
              (else (scan rest)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (first-frame env))))
  (env-loop env))


(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (let ((first (car bindings))
            (rest (cdr bindings)))
        (cond ((null? bindings) (env-loop (enclosing-environment env)))
              ((eq? var (car first)) (set! (cdr first) val))
              (else (scan rest)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (define (scan bindings)
    (let ((first (car bindings))
          (rest (cdr bindings)))
      (cond ((null? bindings) (add-binding-to-frame! var val frame))
            ((eq? var (car first)) (set! (cdr first) val))
            (else (scan rest)))))
  (scan (first-frame env)))