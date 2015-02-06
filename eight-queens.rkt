#lang racket
(define (generate-row x no-columns)
  (let ((number-of-diagonal (integers-in-range x (- (+ x no-columns) 1))))
    (map list
         (n-length-list x no-columns)
         (integers-in-range 1 no-columns)
         number-of-diagonal
         (reverse number-of-diagonal))))

(define (n-length-list element n)
  (if (= n 1)
      (cons element '())
      (cons element (n-length-list element (- n 1)))))

(define (integers-in-range low high)
  (if (= low high)
      (cons low '())
      (cons low (integers-in-range (+ low 1) high))))

(define (eight-queens)
  (let ((q1 (amb (generate-row 1 8)))
        (q2 (amb (generate-row 2 8)))
        (q3 (amb (generate-row 3 8)))
        (q4 (amb (generate-row 4 8)))
        (q5 (amb (generate-row 5 8)))
        (q6 (amb (generate-row 6 8)))
        (q7 (amb (generate-row 7 8)))
        (q8 (amb (generate-row 8 8))));the requirement that they are in different rows is build-in
    (require (distinct-lists? (list q1 q2 q3 q4 q5 q6 q7 q8)))));distinct-lists cares only about the column and diagonal positions


(define (distinct-lists? x)
  (let ((columns (extract-from-lists x (lambda (z) (cadr z))))
        (diagonal1 (extract-from-lists x (lambda (z) (caddr z))))
        (diagonal2 (extract-from-lists x (lambda (z) (cadddr z)))))
    (and (no-repetitions? columns)
         (no-repetitions? diagonal1)
         (no-repetitions? diagonal2))))

(define (no-repetitions? list)
  (cond ((null? list) #t)
        ((= (length list) 1) #t)
        ((member? (car list) (cdr list)) #f)
        (else (no-repetitions? (cdr list)))))

(define (member? x list)
  (cond ((null? list) #f)
        ((eq? x (car list)) 't)
        (else (member? x (cdr list)))))

(define (extract-from-lists x f)
  (if (null? x)
      '()
      (cons (f (car x)) (extract-from-lists (cdr x) f))))

(define a '((1 2 3 5) (3 7 4 8) (5 6 7 2)))
(distinct-lists? a)