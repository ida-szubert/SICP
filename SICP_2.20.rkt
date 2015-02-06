#lang planet neil/sicp

;Exercise 2.20
;dotted-tail notation
;if a parameter list has a dot before the last parameter' then on calling the procedure
;the final parameter's value will be the list of all the arguments that are not bound by other formal parameters
;(define (f x y . x) <body>)
;(f 1 2 3 4 5 6) -> f applied to 1, 2, and (3 4 5 6)

(define (same-parity x . y)
  (if (even? x)
      (cons x (get-even y))
      (cons x (get-odd y))))

(define (get-even l)
  (if (null? l)
      nil
      (if (even? (car l))
          (cons (car l) (get-even (cdr l)))
          (get-even (cdr l)))))

(define (get-odd l)
  (if (null? l)
      nil
      (if (not (even? (car l)))
          (cons (car l) (get-odd (cdr l)))
          (get-odd (cdr l)))))

(define l (list 1 2 3 4 5 6 7 8))
(get-even l)
(get-odd l)
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7 8)
