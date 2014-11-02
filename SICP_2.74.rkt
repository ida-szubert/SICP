#lang racket

;Exercise 2.74

;a.
;e.g. if a personnel-file of one of the divisions was a simple list of records, this function would work:
(define (get-record-divisionX name personnel-file)
  (cond ((null? personnel-file) (error "There is no record for employe" name))
        (else (if (eq? (key (car personnel-file)) name)
                  (car personnel-file)
                  (get-record name (cdr personnel-file))))))

(define (key x) (car x)) ;for example

;a generic function for the headquarters would look-up a division-appropriate procedure for finding the record
;it would retrieve the procedure indexed with the name (or another kind of signature) of the division and the name of the procedure
;this in turn would be applied to the name of the employee
(define (get-record name division)
  ((get 'get-record 'division) name))                 
                  

;b.
(define (get-salary name division)
  (get 'salary ((get 'get-record 'division) name)))

;this procedure first finds the record for the employee
;and then find the salary in the record
;so, each division beeds to have a package of procedures
;they have to include a procedure for finding the record, given a name
;and a procedure for finding the salary, given a record
;these procedures ar entered into a company-wide table, indexed with 'division-name and procedure type (i.e. get-record and salary)

;c.
(define (find-employee-record name divisions)
  (cond ((null? divisions) (error "There is no record fo employee" name))
        ((eq? (get-tag (get-record name (car divisions))) 'record) (get-record name (car divisions)))
        (else (find-employee-record name (cdr divisions)))))

(define (get-tag x)
  (car x))

;for this to work, employee records need to be tagged as records by including 'record as their first element
;what we're doing is trying to find the employee in each division in turn
;by applying a get-record procedure specific to the division
;if no record is found in a division, we'll get an error message
;an error message is not tagged as record
;so we keep looking in other divisions

;d.
;they would need a new package for the acquired company's personnel file
;specifying all the division-specific procedures, such as get-record and salary
;the company-wide table would need a new column to accomodate this new division



;How would a sample package look like?
;Let's assume a division with a straightforwad approach to build a data base
;the personnel file is a simple list of records
;records have the form (record name salary), although there might be sth else in there we're not currently interested in

(define (install-divisionX-package)
  (define (key record) (cadr record))
  (define (salary record) (caddr record))
  (define (get-record name)
    (define (find name file)
      (if (eq? (key (car file)) name)
          (car file)
          (find name (cdr file))))
    (find name our-division-file))
  (put 'salary 'divisionX salary)
  (put 'get-record 'divisionX get-record)
  'done)