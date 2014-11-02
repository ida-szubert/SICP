#lang racket

;--Combining data of different types
;we have treated different types as independent
;but it makes sense to have operations taking more than one type of numbers as arguments
;we could extend e.g. the complex number package with procedures of adding complex and natural numbers
;and put it in the table using the tag (complex scheme-number)

;but it's cumbersome
;also, it undermines modularity
;suddenly when you're writing a package, you need to take account of other packages
;there;s no obvious way of dividing responsibility for mixed-type procedures between packages

;usually, there's a better way
;often, data-types are not completely independent
;there may be ways in which objects of one type may be viewed as being of another type
;that's coercion
;e.g. a natural number is kind of like a complex number whose imag-part is 0

;coercion procedures transform an object of one type into an object of another
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0)) ;;or just n, if we're not using tags for natural numbers

;such transformation procedures are installed in a special coercion table, indexed under the names of the types
;then, apply-generic need to be modified so that when called to apply an operation to arguments
;it checks whether there is an operation defined for the given type(s)
;if not, it tries coercion
;seeing if the first argument can be coerced to the type of the second argument
;if so, we try the operation again
;if not, we try to coerce the other way round
;and returns error message only if it doesn't work either

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))

;there is a possibility that is not exploited by this coercion scheme
;what is neither type1 can be coerced into type2, nor the other way round, but they both can be coerced into a type3?


;--Hierarchies of types
;e.g. real number is a special kind of complex number
;rational number is a special kind of real number
;integer is a special kind of rational number
;this is a particularly straigtforward hierarchy, forming a tower
;so, we needen't write all the possible transformation rules
;we only need to define transformation of integers into rational numbers, rational into real, and real into complex
;the system will figure out what to do to coerce an integer to a complex number
;to take advantage of having such a hierarchy the apply-generic procedure needs to be modified
;each type will have a raise procedure, transforming it into the type one level up
;whenever an operation is called and arguments are not of the same type, the lower one will be successively raised

;also, lower type inherit operations of higher types
;even though we don't define real-part for integers, we can evaluate (real-part 8)
;this would work in the same way as for operations with 2+ arguments
;we raise the argument untill the operation can be applied

;we can also lower data objects, not only raise them
;2+3i + 4-3i is 6
;it would be nice for it to be an integer rather than a complex number 6+0i

;Unfortunately, having a tower-shaped hierarchy is not so common
;it's harder to preserve modularity when the hierarchy of types is nore complex

;Exercise 2.81
;a.
;if we try to call exp with two complex number, apply-generic willfirst look for an exp operation indexed with (complex complex)
;we haven't put such an operation in the table, so it will find nothing
;with Louis's procedures installed, apply-generic will try to "coerce" type complex to type complex and succeed
;then, it will look again in the table for exp indexed with (complex complex)
;it won't find it
;so it will try coercion of complex to xomplex, succeed again, look for the operation again...
;evaluation will never stop
;and even if it did, we gain nothing by Louis's procedures, because his "coercions" don't change a thing, and an appropriate operation
;won't appear in the table out of thin air

;b.
;Apply-generic is fine as it is.
;If ther's no operation defined for two arguments of type1, apply-generic will try coercion, but there won't be any coercion procedures to apply
;and in the end it will return the truthful error message "No method for these types"

;c.
(define (modified-apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)    
                    (error "No method for these types" (list op type-tags))  ;;if there are two arguments, and they are of the same type, return error
                    ;;and if there are two arguments of different types, try coercion
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2 (modified-apply-generic op (t1->t2 a1) a2))
                            (t2->t1 (modified-apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types" (list op type-tags)))))))
              (error "No method for these types" (list op type-tags)))))))  ;;this error is here because we only consider operations with 2 arguments

           

;Exercise 2.82
;generalize apply-generic to handle multiple arguments
(define (multi-apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (all-the-same? type-tags)
              (error "No method for these types" (list op type-tags))
              ((define (coerce-list tag l)
                 (if (eq? tag (type-tag (car l)))
                     (cons (cadr l) (coerce-list tag (cdr l)))
                     (if (get-coercion (type-tag (car l)) tag)
                         (cons ((get-coercion (type-tag (car l)) tag) (car l)) (coerce-list tag (cdr l)))
                         #f)))
               (define (recoursively-coerce list)
                 (if (null? list)
                     (error "No method for these types" (list op type-tags))
                     (if (pair? (coerce-list (type-tag (car list)) list))
                         (coerce-list (type-tag (car list)) list)
                         (coerce-list (type-tag (cadr list)) list))))
               multi-apply-generic op (recoursively-coerce args)))))))


;Exercise 2.83
;in the scheme-number package:
(define (raise-integer n)
  (make-rational n 1))
(put 'raise 'scheme-number raise-integer)

;in the rational package
(define (raise-rational n) ;;I'm not sure, but real numbers would probably be stored as floating point numbers
  (attach-tag 'real (/ (numer n) (denom n))))
(put 'raise 'rational raise-rational)

;out there, as long as we haven't got a real package
(define (raise-real n)
  (make-complex-from-real-imag n 0))
(put 'raise 'rational raise-rational)

(define (raise n)
  (apply-generic 'raise n))
;apply-generic applies the procedure found in the table to the contents of n, that's why specific definitions assume n is a tagless number

                 
               
             
          
                            
                                 
                        
              

