#lang racket

;Exercise 4.7
(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))

(define product
 (let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y 5)))
      (* x z)))))

product

(define (let*->nested-lets exp)
  (define (help bindings-list)
    (if (null? bindings-list)
        (body exp)
        (let ((first (car bindings-list))
              (rest (cdr bindings-list)))
          (make-let (car first)
                    (cadr first)
                    (help rest)))))
  (help (bindings exp)))

(define (body let*-exp)
  (caddr let*-exp))

(define (bindings let*-exp)
  (cadr let*-exp))

(define (make-let variable value body)
  (cons 'let (cons (cons (cons variable (cons value '())) '()) (cons body '()))))



(make-let 'y '(+x 2) '(* x z))

(body '(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z)))

(let*->nested-lets '(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z)))
