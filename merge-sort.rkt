#lang racket

(define (merge-ordered-lists l1 l2 p)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((p (car l1) (car l2)) (cons (car l1) (merge-ordered-lists (cdr l1) l2 p)))
        (else (cons (car l2) (merge-ordered-lists l1 (cdr l2) p)))))

(merge-ordered-lists '(1) '(9) <)

(define (elements->singletons l)
  (map (lambda (x) (cons x '())) l))

(elements->singletons '(1 3 7 5 9 8 4))

(define (pairwise-order l p)
  (if (null? l)
      '()
      (let ((first (car l)))
        (if (null? (cdr l))
            (cons (car l) '())
            (let ((second (cadr l)))
              (cons (merge-ordered-lists first second p) (pairwise-order (cddr l) p)))))))

(pairwise-order (elements->singletons '(1 3 7 5 9 8 4)) <)
(pairwise-order (pairwise-order (elements->singletons '(1 3 7 5 9 8 4)) <) <)
(pairwise-order (pairwise-order (pairwise-order (elements->singletons '(1 3 7 5 9 8)) <) <) <)

(define (sort1 list predicate)
  (define (inner-sort l p)
    (if (= (length l) 1)
        (car l)
        (inner-sort (pairwise-order l p) p)))
  (inner-sort (elements->singletons list) predicate))

(sort1 '(1 3 7 5 9 8 4) <)

(define (sort list predicate)
  (define (elements->singletons l)
    (map (lambda (x) (cons x '())) l))
  (define (merge-ordered-lists l1 l2 p)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((p (car l1) (car l2)) (cons (car l1) (merge-ordered-lists (cdr l1) l2 p)))
          (else (cons (car l2) (merge-ordered-lists l1 (cdr l2) p)))))
  (define (pairwise-order l p)
    (if (null? l)
        '()
        (let ((first (car l)))
          (if (null? (cdr l))
              (cons (car l) '())
              (let ((second (cadr l)))
                (cons (merge-ordered-lists first second p) (pairwise-order (cddr l) p)))))))
  (define (inner-sort l p)
    (if (= (length l) 1)
        (car l)
        (inner-sort (pairwise-order l p) p)))
  (inner-sort (elements->singletons list) predicate))



(define (->string obj)
    (if (symbol? obj)
        (symbol->string obj)
        (number->string obj)))
(map string->symbol
     (sort
      (map (lambda (x) (->string x)) '(aubergine onion cucumber sourkrout beetroot carrot parsley kiwi pear celery celeriac marrow apple potato yam))
      string<?))



(define (alphabetic-order? a b)
  (string<? (->string a) (->string b)))

(sort '(aubergine onion cucumber sourkrout beetroot carrot parsley kiwi pear celery celeriac marrow apple potato yam) alphabetic-order?)