#lang racket

;Exercise 4.12
;set-variable-value!, define-variable!, and lookup-variable-value! all have a similar structure

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

 (define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set! (car vals) val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (apply-to-vals var proc frame)
  (define (scan vars vals)
    (cond ((null? vars) #f)
          ((eq? (car vars) var) (proc vals))
          (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame) (frame-values frame)))

(define (idn x) x)

(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (let ((result (apply-to-vals var idn frame))) ;; don't do anything if found, just return the list
        (if result
            (car result)
            (lookup-variable-value var (enclosing-environment env)))))))
