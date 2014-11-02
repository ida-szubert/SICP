#lang racket
;why is the data abstraction discussed to this point not powerful enough?
;we have isolated underlying representations of data-ojects ,e.g. rational numbers, from the way these objects are used
;by using make-rat, numer, denom we ensure that no matter how rationals are implemented in terms of lists, the programs will work
;as long as we have defined the constructor (make-rat) and selectors (numer, denom) in an appropriate way

;but we might want a system that uses multiple underlying representations
;e.g. when there are situations where one representation is more appropriate then the other and vice versa
;that's the case with complex numbers
;they can be thought of as a real and imaginary part or as magnitude and angle

;we need abstraction barriers to isolate different representation from each other
;also, it would be nice to use pre-existing modules without the need of redesigning them

;GENERIC PROCEDURES
;can operate on data that may be represented in more than one way
;working in terms of data objects that have type tags
;data-directed programming

;EXAMPLE: COMPLEX NUMBERS
;rectangular form
;real coordinate and imaginary coordinate
;addition reduces to addition of the coordinates

;multiplication is easier to express in terms of polar form
;the product of c1and c2 would be obtained by
;streching c1 by the magnitude of c2, and then rotating it by the angle of  c2
;so you multiply magnitued and add angles

;we assume that there are 4 selectors: real-part, imag-part, magnitude, angle, and 2 constructors
;for any z
;make-from-real-imag (real-part z) (imag-part z)
;make-from-mag-ang (magnitude z) (angle z)
;these procedures both produce complex numbers equal to z

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
(make-from-mag-angle (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))

;selectors and constructors based on the rectangular form
;(define (real-part z) (car z))
;(define (imag-part z) (cdr z))
;(define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z))))) 
;(define (angle z) (atan (imag-part z) (real-part z)))
;(define (make-from-real-imag x y) (cons x y))
;(define (make-from-mag-ang r a) (cons (* r (cos a)) (* r sin a)))

;selectors and constructors based on the polar form
;(define (real-part z) (* (magnitude z) (cos (angle z))))
;(define (imag-part z) (* (magnitude z) (sin (angle z))))
;(define (magnitude z) (car z))
;(define (angle z) (cdr z))
;(define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
;(define (make-from-mag-ang r a) (cons r a))

;the arithmetic operations will work with both representations

;it is possible to use both representations in one system
;but we need a way to distinguish data in polar from data in rectangular
;by attaching type tags

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;now, both representations can be used
;the caveat is that make procedures must add a tag
;also, procedures have to have different names

;selectors and constructors based on the rectangular form
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z) (sqrt (+ (square (real-part-rectangular z))
                                           (square (imag-part-rectangular z))))) 
(define (angle-rectangular z) (atan (imag-part-rectangular z)
                                    (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y) (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a) (attach-tag 'rectangular (cons (* r (cos a)) (* r sin a))))

;selectors and constructors based on the polar form
(define (real-part-polar z) (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z) (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y) (attach-tag 'polar (cons (sqrt (+ (square x) (square y))) (atan y x))))
(define (make-from-mag-ang-polar r a) (attach-tag 'polar (cons r a)))

;then, we should define generic selectors, because no one wants to bother with whether their data are polar or rectangular
;selectors have different behaviors depending on the tags of the data
(define (real-part z)
  (cond ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (else (error "Unknown type: MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))

;the arithmetic procedures need almost no change
;they call the generic selectors, and they don't care about therepresentation of the data
;but they construct complex numbers, and we must decide on the representation of these constructed numbers

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))
;that seems reasonable


;DATA-DIRECTED PROGRAMMING

;dispatching on type - the strategy of checking the type of a datum and calling an appropriate procedure
;the above is one implementation of dispach on type
;real-part, imag-part, magnitude, and angle must know about all the representations, so if you add a type, you need to modify these operations
;it's also bothersome that we need to take care that no two procedures have the same name

;imagine a two-dimensional table
;with possible operations on one axis, and possible types on the other
;the entries in this table are procedures that implement each operation for each type
;so, the interface between complex-arithmetics and different representations will be a single procedure that looks up the appropriate entry in the table
;and when we introduce a new type, nothing needs to change apart from the fact that the table needs to be extended
;(let's assume) we've got two useful operations
;(put operation type item) sticks the item in the table, in a position indexed with the operation and the type
;(get operation type) looks up the entry indexed with the operation and the type

;what helps with the naming problem is that the original procedures written for polar and rectangular representations are now enclosed within packages
;there's a polar package, which includes, as internal definitions, all definitions of operations using polar representation
;the same goes for rectangular representation
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) (sqrt (+ (square real-part z) (square imag-part z))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a)))) ;these are the internal procedures
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y)))) ;so that constructed data-points will be tagged with type of representation
  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))
  'done) ;the second part specifies the interface to the rest of the system

;'(rectangular) rather than 'rectangular is used with selectors. Why?
;because selectors can have more than one argument, and the arguments can have different types (though here they don't)
;constructors, on the other hand, always make an object of a particular kind

;So, how do more complex operations works when we use a table rather than a list of definitions?
;the table can be accessed by a general operation
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
     (let ((proc (get op type-tags)))
       (if proc
           (apply proc (map contents args))
           (error "No method for these types: APPLY-GENERIC" (list op type-tags))))))

;since it's a generic operation, we don't know in advance how many arguments there'll be
;apply-generic looks in the table under (a) the name of the operation and (b) the types of the arguments
;apply is a primitive procedure, which takes a procedure and a list, and applies the procedure to the elements of the list
;apply-generic need to get tagged arguments (apart from the first one)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;these definitions don't have to change if we add a new type of representation

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))




