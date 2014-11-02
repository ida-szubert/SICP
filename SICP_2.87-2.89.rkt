#lang racket

;Symbolic Algebra

;start with a set of primitive objects- constants and variables
;these can be combined with algebraic operators
;typical abstractions in symbolic algebra are linear combination, polynomial, rational function, trigonometric function
;these can be also thought of as types
;e.g.
;x^2 sin (y^2 + 1) + x cos 2 y + cos (y^3 - 2 y^2)
;is a polynomial in x with coefficients that are trigonometric functions of polynomials in y whose coefficients are integers

;writing a system for symbolic algebra is a tough task


;Arithmetics of polynomials
;including only addition and multiplication
;polynomial- a sum of terms, each of which is either a coefficient, a power of the indeterminate, or a product of the two
;a coefficient is an algebraic expression not dependent upon the indeterminate of the polynomial
;this means that a coefficient can well be a polynomial as well, dependent on some other indeterminate
;there are other ways of representing polynomials, but they won't be used here

;so, how exactly will polynomials be represented?
;as a list of the variable and the list of terms
;there will be selectors variable and term-list
;and a constructor make-poly
;variables can be compared using same-variable?
;we do that becaue only polynomials with the same indeterminate will be added or multiplied

(define (same-variable? v1 v2)
  (and (symbol? v1) (symbol? v2) (eq? v1 v2)))

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1) (add-terms (term-list p1) (term-list p2)))
      (error "Polynomials not in the same var: ADD-POLY" (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (varaible v1) (variable v2))
      (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
      (error "Polynomials not in the same var: MUL-POLY" (list p1 p2))))


(define (install-polynomial-package)
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
  ;; adjoin-term
  ;; add-poly
  ;; used by add-poly
  ;; mul-poly
  ;; used by mul-poly
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  'done)

;Addition:
;so, how do we add terms?
;terms of the same order must be combined
;the new term will be of the same order, and its coefficient will be the sum of the coefficients of the addends
;things we need:
;constructors empty-list and adjoin-term
;and a predicate empty-termlist?
;selectors first-term and rest-terms
;constructor make-term (constructs a term given the order and the coefficient
;selectors order and coeff

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2)) (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                 (else (adjoin-term (make-term (order t1) (add (coeff t1) (coeff t2))) (add-terms (rest-terms L1) (rest-terms L2)))))))))

;we don't know what kinds of expressions will coefficients be
;we use a generic add operation, which should work for integers, rational numbers, complex numbers, and polynomials

;Multiplication
;multiply each term of the first list by all the terms of the other list
;add up all the resulting term lists
;how do you multiply two terms?
;make a term whose order is the sum of the orders of t1 and t2 and whose coefficient is the product of the coefficients of t1 and t2

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2) (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
                     (mul-term-by-all-terms t1 (rest-terms L))))))


;How to represent term lists?
;it's a sect of coefficients keyed by the order of the term
;add-terms and mul-term access term lists sequentially, from the highest order to the lowest
;so, we want termlist representation to be ordered
;what also matters is whether a polynomial is dense or spare (i.e. whether it has nonzero coefficients in terms of most orders)
;if it's dense, termlist is best represented as a list of coefficients
;then, the order of a particular term is equal to the length of the sublist beggining after that term's coefficient
;for sparse polynomials it's better to have a list of pairs (order, coeff) - otherwise the would be an awful lot of 0s
;as most calculations are performed with sparse polynomials, we'll represent term list as a list of pairs

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term)) ;;it's assumed that integer's don't have tags, as was implemented in one of the exercises
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;Exercise 2.87

(define (=zero? x)
  (apply-generic '=zero? x))

;for natural numbers
(put '=zero? 'scheme-number zero?)

;for rational numbers
(define (=zero?-rat x)
  (= (num x) 0))
(put '=zero? 'rational =zero?-rat)

;for rectangular complex numbers
(define (=zero?-rect x)
  (and (= (real-part x) 0) (= (imag=part x) 0)))
(put '=zero? 'rectangular =zero?-rect)

;for polar complex numbers
(define (=zero?-polar x)
  (or (= (magnitude x) 0) (= (angle x) 0)))
(put '=zero? 'polar =zero?-polar)

;in the complex package
(define (=zero?-complex x)
  (apply-generic '=zero? x))
(put '=zero? 'complex =zero?-complex) 

;for polynomials
;polynomial is 0 when all terms are 0; term is 0 when the coeffcient is 0; when a coefficient is 0, we don't include the term in the termlist
;so, polynomial is 0 when the termlist is empty
(define( =zero?-poly x)
  (null? (termlist x)))
(put '=zero? 'polynomial =zero?-poly)


;Exercise 2.88
;subtraction of polynomials
;subtracting x is the same as adding (neg x)
;subtracting polynomial y from x we need to add the terms of x to - (terms of y)
;coefficient of each term of y needs to be multiplied by (-1)

(define (neg-poly p)
  (make-polynomial (variable p) (neg-terms (termlist p))))

(define (neg-terms term-list)
  (if (empty-list? term-list)
      '()
      (let ((coeff1 (coeff (first-term term-list)))
            (order1 (order (first-term term-list))))
        (cons (make-term order1 (neg coeff1)) (neg-terms (cdr term-list))))))

(define (neg-scheme-number n)
  (* -1 n))
(define (neg-rational n)
  (make-rational (* -1 (numer n)) (denom r)))
(define (neg-complex n)
  (make-complex-from-real-imag (* -1 (real-part n)) (* -1 (imag-part n))))

;does (* -1 x) work for all types of numbers, or should I write a generic neg procedure, redirecting to neg specific for the type of the coefficient?
;ok for integers
;ok for rational
;(neg x) + x should be zero
;not ok for complex; real and imaginary parts of a complex number should be both multiplied by -1

  
;Exercise 2.89
(define (first-term t) (make-term (- (length t) 1) (car t)))
(define (rest-terms t) (cdr t))
(define (order term ) (car term))
(define (coeff term) (cadr term))
(define (new-adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons (coeff term) term-list)))
