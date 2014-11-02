#lang racket
;Exefcise 2.84

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

(define (apply-generic-with-raising op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (cond ((eq? type1 type2) (error "No method for these types" (list op type-tags)))
                      ((higher? type1 type2) (apply-generic-with-raising op (list a1 (raise a2))))
                      (else (apply-generic-with-raising op (list (raise a1) a2)))))
              (error "No method for these types" (list op type-tags)))))))

(define tower '(complex real rational scheme-number))

(define (member? x list)
  (cond ((null? list) #f)
        ((eq? x (car list)) #t)
        (else (member? x (cdr list)))))

(define (higher? t1 t2)
  (define (help t1 t2 list)
    (if (null? list)
        #f
        (if (and (and (member? t1 list) (member? t2 list)) (not (member? t1 (cdr list))))
            #t
            (help t1 t2 (cdr list)))))
  (help t1 t2 tower))

(higher? 'complex 'rational)
(higher? 'scheme-number 'real)
                    
                    