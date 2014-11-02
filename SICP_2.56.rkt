#lang racket

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2) (eq? v1 v2))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

;Exercise 2.57
(define (addend s) (cadr s))
(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplier p) (cadr p))
(define (multiplicand p)
  (if (null? (cddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 . a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2)))) 

;Exercise 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp) var))
                                  (make-product (deriv (multiplier exp) var)
                                                (multiplicand exp))))
        ((exponentiation? exp) (make-product
                                (make-product (exponent exp)
                                              (make-exponentiation (base exp) (- (exponent exp) 1)))
                                (deriv (base exp) var)))                      
        (else (error "unknown expression type: DERIV" exp))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))

(define (make-exponentiation b exp)
  (cond ((=number? exp 1) b)
        ((=number? exp 0) 1)
        ((and (number? b) (number? exp)) (exp b exp))
        (else (list '** b exp))))

(define (exponent x) (caddr x))
(define (base x) (cadr x))

(deriv '(** x 2) 'x)

;Exercise 2.58
;modify the representation of expressions so that it mirrors ordinary mathematical notation
;a. assume that all expressions are fully parenthesized and + and * always take two arguments

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 . a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;b. allow for dropping unnecessary parantheses, assuming that * is done before +
;TODO










