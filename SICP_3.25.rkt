#lang planet neil/sicp

;Exercise 3.25
;how to implement a table in which values are stored under an arbitrary number of keys?
;in which, additionally, different values can be stored under different number of keys

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (let ((subtable local-table)))
        (cond ((null? (cdr key-list))
               (let ((record (assoc (car key-list) (cdr subtable))))))
              (else (set! subtable (assoc (car key-list) subtable))
                    (lookup (cdr key-list)))))
    (define (insert! key-list value)
      (let ((subtable local-table))
        (cond ((null? (cdr key-list))
               (let ((record (assoc (car key-list) subtable)))
                 (if record
                     (set-cdr! record value)
                     (set-cdr! subtable (cons (cons (car key-list) value) (cdr subtable))))))
              (else (set! subtable (assoc (car key-list) subtable))
                    (insert! (cdr key-list) value)))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))               
                
;so, given a list of keys, how do we find the record?
;we look at the key list
;if there's only one key, we use assoc on that key and the current table
;if there are more, we look for the subtable using (car key-list) and the current table
;and use this subtable and (cdr key-list) to repeat the procedure untill there's only one key left
;so, we need to keep track of the current subtable

;given a list of keys, how do we insert a value into a table?
;we look at the key list
;if there's only one key, we insert (cons key value) into the current table
;if there are more, we create/find the subtable matching (car key-list)