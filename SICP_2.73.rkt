#lang racket


(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (eq? v1 v2))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))

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

;Exercise 2.73
(define (original-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp) (original-deriv (multiplicand exp) var))
                   (make-product (multiplicand exp) (original-deriv (multiplier exp) var))))
        (else (error "unknown expression type: DERIV" exp))))

;you can think of it as dispaching procedures on the type of the expression being differentiated
;the algebraic operators serve as type tags of sum and product
;and the operation is deriv

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;a.
;if an expression to be differentiated is a number or a variable, it doesn't have a tag (since tags are arythmetic operation symbols)
;therefore, there's no way for the table of procedures to contain procedures for differentiating numbers and variables
;apart from these two cases, we're saying that to differentiate an expression given a variable, 
;we need to get from the table the entry indexed with 'deriv and the operator of the expression, i.e the type tag
;the procedure that we've found is applied to the operands of the expression and the variable
;we presume the original representation from the differentiation exercise
;that is, expressions are represented as a mathematical operator followed by the arguments

;b.
(define (install-sum-package)
  (define (addend x) (car x))
  (define (augend x) (cadr x))
  (define (deriv exp var) (make-sum (deriv (addend sum) var)
                                    (deriv (augend sum) var)))
  (put 'deriv '+ deriv)
  'done)

(define (install-mul-package)
  (define (multiplier x) (car x))
  (define (multiplicand x) (cadr x))
  (define (deriv exp var) (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
                                    (make-product (multiplicand exp) (deriv (multiplier exp) var)))
                                    (deriv (augend sum) var))
  (put 'deriv '* deriv)
  'done)
;added and augend, as well as multiplier and multiplicand, are defined slightly differently that before
;that's because the generic deriv operation applies the procedure pulled from the table to the operands of the expression
;rather than the whole expression as it was before
;so, we don't need to do anything to omitt the operation sign, because it isn't there

;make-sum and make-product need not be inside the packages. These procedures are not dependent on what kind of expressions they are fed
  (define (make-product m1 m2)
    (cond ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* a1 a2))
          (else (list '* a1 a2))))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

;c.
(define (install-expo-package)
  (define (exponent x) (cadr x))
  (define (base x) (car x))
  (define (deriv exp var)
    (make-product (make-product (exponent exp)
                                (make-exponentiation (base exp) (- (exponent exp) 1)))
                  (deriv (base exp) var)))
  (put 'deriv '** deriv)
  'done)


(define (make-exponentiation b exp)
  (cond ((=number? exp 1) b)
        ((=number? exp 0) 1)
        ((and (number? b) (number? exp)) (exp b exp))
        (else (list '** b exp))))

;d.
;what if the dispatch line in deriv was
;((get (operator exp) 'deriv) (operands exp) var)

;the only necessary change to the packages would be to the <put> procedure, changing the order of 'deriv and '**
