#lang racket

;How to build tables as mutable list structures?
;first, a one-dimensional table, with entries stored under a single key

;list of records
;each record isa pair consisting of a key and the associated value
;glued together by cons
;i.e. a list of pairs, whose cars point to records, and cdrs point to the next pair
;table is a headed list, i.e. has a special pair at the beginning which hold a dummy record, which is an arbitrary smbol
;it's there because we need a record spot to change when we want to insert a new record without affecting other records

;extracting from the table:
;(lookup key table)
;-> returns value associated with the key or #f
(define (lookup key table)
  (let ((record (assoc key (cdr table)))) ;assoc doesn't see the dummy record which has no key
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))


;so, if assoc finds a record with a given key, lookup extracts the value of that record
;lookup also checks whether the ouput of assoc is not #f rather than an actual record

;Inserting a record
;first check whether there already is a record with agiven key in the table
;use assoc
;if not, cons the key onto the value
;and insert it after the dummy record
;if yes, change the value associated with that key
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))


;Now, two dimensional tables
;it may be thought of as a table in which each key identifies a subtable
;so, the main table has as many records as there are subtables
;the subtable don't have a special header symbol, since this function is performed by the key of the subtable)
(define (lookup2d key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

;inserting into a 2D table
;see if there's a subtable stored under key-1
;if not, build it and inset into the table
;if yes, insert the new record into it

(define (insert2d! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
        (set-cdr! table
                  (cons (list key1 (cons key2 value)) (cdr table)))))
  'ok)

;lookup and insert have a table as a variable, hence they can be used with different table
;one could also have separate procedures for each table
;but why would you want sth like that?
;a table can be represented procedurally, as an object that maintains an internal table as part of its local state
;you send a message to this table object and it replies by providing requested info

(define (make-2Dtable)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! local-table (cons (list key-1 (cond key-2 value)) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-2Dtable))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
;both get and put access the same local table, stored within the object called operation-table

;Exercise 3.24
;assoc uses equal? as a test of equality of keys
;but keys might be numeric, and we might not need an exact match, only a match with a specified degree of tolerance
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! local-table (cons (list key-1 (cond key-2 value)) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
  dispatch))           

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
