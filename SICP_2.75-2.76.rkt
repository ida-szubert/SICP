#lang racket

;Message passing

;instead of having general operations which dispatch specific procedures depending on the type of arguments
;we might use general data objects, which do different thingsdepending on the type of operation applied
;let's look at a complex number in a rectangular form

(define (make-from-real-img x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (sqquare y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispach)

;essentialy, instead of producing a complex number, make-from-real-imag becomes a function
;it takes as its argument an operation type
;and depending on it, it outputs different values- it's real or imaginary part, or magnitude, or angle

(define (apply-generic op arg) (arg op))

;if we want to get the real part of a complex number, we (apply-generic real-part x)
;let's say (define x (make-from-real-imag 3 4))
;by apply-generic we are calling (dispatch real-part)
;and we'll get the result 3

;Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eg? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error  "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

;Exercise 2.76

;explicit dispatch:
;When new types are added, e.g. a new complex number representation, every procedure, such as real-part or make-from-mag-ang
;would need to be extended to accomodate the fact that there is a third kind of representation
;When we need a new operation, we would need to write one, which accounts for all possible types of data used in the system

;data-directed style:
;Adding a new type requires writing a new set of procedures, specific to that type, and interfacing them with the generic procedures
;and extending the table by a new column
;Adding a new operation requires modifying all the packages by writing type-specific versions of these operations, 
;and extending the table by a new row

;message-passing:
;Ading a new type requires writing a set of new data-objects/procedures that would specify what happens when you apply your procedures to
;this new data-type
;Adding a new operation would require modifying every existing data-type in order to specify what happens uppon calling this procedure with every
;type of argument in the system

;It's better to write procedures from scratch than to modify existing ones. It's difficult to keep track of every procedure that needs to be modified,
;so it's easier to make mistakes.
;If you mostly need to make changes to the types the system uses, it's probably best to use data-directed style, as it requires you to write
;a self-contained package for that type and interface it with the system
;you don't have to worry about missing some existing procedure that should have been modified.
;If you mostly need to make changes to the operations, data-directed style is not so great, because you'd need to modify packages for all data-types.
;Message-passing is better, TODO