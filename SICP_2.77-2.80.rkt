#lang planet neil/sicp

;Exercise 2.77
;we've written a general arithmetic package
;numbers used in this system are tagged
;in case of complex numbers they have two tags
;the outer one says i'm a complex number,
;and the inner one says i'm in polar/rectangular representation

;what happens when we want to evaluate (magnitude z)?
;we know from the outer tag that z is a complex number
;so we go looking for a table entry indexed with 'complex and 'magnitude
;but there's no magnitude operation for complex numbers
;we have only defined magnitude for numbers tagged 'polar or 'rectangular
;the internal procedures of the complex package use real-part, imag-part, magnitude, and angle procedures from the polar and rectangular packages
;if we asked for (magnitude (cdr z)) we would get the expected answer

;if we want (magnitude z) to work, we need to put selector procedures in the table indexed by type 'complex

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ;;here come the selectors
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

;the procedures that we put in the table are generic; they are a call to apply-generic, which finds a table entry under 'real-part'polar/'rectangular

;(define z (cons 'complex (cons 'rectangular (cons 3 4))))
;(magnitude z)

;(apply (get 'magnitude 'complex) z)
;(magnitude '('rectangular 3 4))
;(apply (get 'magnitude 'rectangular) '(3 4))
;(5)


;Exercise 2.78
;in section 2.4.2:
(define (old-attach-tag type-tag contents)
  (cons type-tag contents))

(define (old-type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (old-contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;we want to modify these so that natural numbers don't need a tag

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (symbol? datum)
      (if (number? datum)
          'scheme-number
          (error "Bad tagged datum: TYPE-TAG" datum))
      (car datum)))

(define (contents datum)
  (if (symbol? datum)
      (if (number? datum)
          datum
          (error "Bad tagged datum: CONTENTS" datum))
      (cdr datum)))

(define x 10)
(type-tag x)


;Exercise 2.79

(define (equ? x y) (apply-generic 'equ x y))

;for natural numbers
(put 'equ? '(scheme-number scheme-number) =)
     
;for rational numbers, assuming that eq? is #t when x and y are identical, not when they have the same value
(define (equ? x y) (and (= (numer x) (numer y)) (= (denom x) (denom y))))
(put 'equ? '(rational rational) equ?)

;for complex numbers with rectangular and with polar representation
;i've made them separate to reduce the risk of error
;when rectangular complex number is transformed into a polar one, and vice versa, some amount of uncertainty is introduced by the calculations
;it might be the case that two numbers are equal, but the rounding-off would make them apear not to be
;so, these procedures would go in the rectangular and polar packages
(define (equ? x y) (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
(put 'equ? '(rectangular rectangular) equ?)
(put 'equ? '(rectangular polar) equ?)

(define (equ? x y) (and (= (magnitude x) (magnitude y)) (= (angle x) (angle y))))
(put 'equ? '(polar polar) equ?)
(put 'equ? '(polar rectangular) equ?)
;and this procedure would go in the complex package
(define (equ? x y)
  (apply-generic 'equ? x y))
;what would happen if we wanted to compare two complex numbers with different representations?
;probably we need to make an arbitrary decision, e.g. follow the type of x

;we don't need to worry about what happens when we try to compre two different types of numbers
;apply-generic will take care of the error message



;Exercise 2.80
(define (=zero? x)
  (apply-generic '=zero? x))

;for natural numbers
(put '=zero? 'scheme-number zero?)

;for rational numbers
(define (=zero? x)
  (= (num x) 0))
(put '=zero? 'rational =zero?)

;for rectangular complex numbers
(define (=zero? x)
  (and (= (real-part x) 0) (= (imag=part x) 0)))
(put '=zero? 'rectangular =zero?)

;for polar complex numbers
(define (=zero? x)
  (or (= (magnitude x) 0) (= (angle x) 0)))
(put '=zero? 'polar =zero?)

;in the complex package
(define (=zero? x)
  (apply-generic '=zero? x))
(put '=zero? 'complex =zero?) 


      
