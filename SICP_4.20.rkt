#lang racket

;Exercise 4.20
;a
(define (letrec->let exp)
  (let ((variables (extract-letrec-variables (letrec-bindings exp)))
        (values (extract-letrec-values (letrec-bindings exp))))
  (make-let variables
            (*unassigned*-list (length variables))
            (append (make-assignment variables values)
                    (letrec-body exp)))))

(define (letrec-bindings exp)
  (cadr exp))

(define (letrec-body exp)
  (cddr exp))

(define (extract-letrec-variables bindings)
  (if (null? bindings)
      '()
      (cons (caar bindings) (extract-letrec-variables (cdr bindings)))))

(define (extract-letrec-values bindings)
  (if (null? bindings)
      '()
      (cons (cadar bindings) (extract-letrec-values (cdr bindings)))))

(define (*unassigned*-list x)
  (define (help counter)
    (if (eq? counter x)
        '()
        (cons '*unassigned* (help (+ counter 1)))))
  (help 0))

(define (make-assignment variables values)
  (define (assignment variable value)
    (list 'set! variable value))
  (if (eq? (length variables) (length values))
      (if (null? variables)
          '()
          (cons (assignment (car variables) (car values))
                (make-assignment (cdr variables) (cdr values))))
      (error "Unequal amount of variables and values to be assigned")))

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

(define sample-letrec
  '(letrec ((even? (lambda (n)
                     (if (= n 0) #t (odd? (- n 1)))))
            (odd? (lambda (n)
                     (if (= n 0) #f (even? (- n 1))))))
     <rest-of-body>))


(letrec->let sample-letrec)
