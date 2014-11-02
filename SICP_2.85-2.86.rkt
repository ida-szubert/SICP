#lang racket

;Exercise 2.85
;from complex to real
(define (project-complex n)
  (make-rational (real-part n) 1))
(put 'project 'complex project-complex)

;from real to rational -thet's not clear, besides, there's no rational package anyway 
;that's why i'm going to project complex to rational

;from rational to integers
(define (project-rational n)
  (round (/ (numer n) (denom n))))
(put 'project 'rational project-rational)

(define (pushable? n)
  (and (not (eq? (type-tag n) 'scheme-number))
       (equ? (raise (project n)) n)))
;this predicate will not work with my previous definitions
;raise and push hierarchies need to be the same, and right now I've got raising to and from real, but not pushing to and from real

(define (drop n)
  (if (pushable? n)
      (drop (apply-generic project n))
      n))




(define (apply-generic-with-raising op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
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
  

;Exercise 2.86
TODO, i'm too fed up