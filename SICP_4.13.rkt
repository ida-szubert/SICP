#lang racket

;Exercise 4.13
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (error "Variable not bound in this environment:" var))
            ((eq? var (car vars)) ((remove-head! vars)
                                   (remove-head! vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (remove-head! list)
  (set-car! list (cadr list))
  (set-cdr! list (cddr list)))