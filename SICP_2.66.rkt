#lang racket

;information retrieval
;3 ways of representing sets as lists have been examined
;to show that choice of representation has an impact on performance of the programs
;and because these techniques appear all over the place in IR

;let's consider a data base with a large number of records, e.g. company personnel files
;a data-management system will spend much time accessing and modifying the records
;it needs an efficient method for performing its  tasks
;for this reason, we use keys
;a key can be anything that uniquely identifies a record
;i.e. personal ID card number for each employee
;so, when writing representation for the records, we should include a key selector procedure
;the key can be an otherwise usefull part of the record
;but it could also be generated during record-construction specially for the purpose of being the key

;if we want to find a record, we use a lookup procedure, which takes a key and a data base as its arguments

;typically, if records will be randomly accessed, the set of records is implemented by a tree-based method

;Exercise 2.66

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (lookup given-key data-base)
  (cond ((null? data-base) #f)
        ((= given-key (key (entry data-base))) (entry data-base))
        ((< given-key (key (entry data-base))) (lookup key (left-branch data-base)))
        ((> given-key (key (entry data-base))) (lookup key (right-branch data-base)))))

(define (key record) (car record))
(define (make-record id name) (cons id name))

(define data-base '((2 ) ((1 . Bill) () ()) ((3 . Frank) () ((4 . John) () ()))))