#lang racket
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length-iter items)
  (define (help items count)
    (if (null? items)
        count
        (help (cdr items) (+1 count))))
  (help items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;Exercise 2.17
(define (last-pair l)
  (let ((last-element (list-ref l (- (length l) 1))))
    (cons last-element '())))

(define list (cons 2 (cons 4 (cons 6 (cons 8 (cons 10 (cons 12 '())))))))
(last-pair list)

;Exercise 2.18
(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (cons (car l) '()))))

  
(reverse list)