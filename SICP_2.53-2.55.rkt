#lang racket
;Exercise 2.53

;(list 'a 'b 'c) -> (a b c)
;(list (list 'george)) -> ((george))
;(cdr '((x1 x2) (y1 y2))) -> ((y1 y2))
;(cadr '((x1 x2) (y1 y2))) -> (y1 y2)
;(pair? (car '(a short list))) -> #f
;(memq 'red '((red shoes) (blue socks))) -> #f
;(memq 'red '(red shoes blue socks)) -> (red shoes blue socks)

;memq checks if its argument is a member of the list; if so, it returns the portion of the list starting with the element in question

;Exercise 2.54

(define (atom? x) (not (pair? x)))

(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((and (atom? (car l1)) (atom? (car l2)))
         (if (eq? (car l1) (car l2))
             (equal? (cdr l1) (cdr l2))
             #f))
        ((or (atom? (car l1)) (atom? (car l2))) #f)
        (else (if (equal? (car l1) (car l2))
                  (equal? (cdr l1) (cdr l2))
                  #f))))

(define l1 '((a small) feisty tiger))
(define l2 '((a small) feisty))
(equal? l1 l2)

;Exercise 2.55
(car ''abracadabra)

;it evaluates as quote, because (car (quote (quote abracadabra)) is the word quote
;that's because we're quoting the list (quote abracadabra), hence the second 'quote' is not a call to the quote procedure, but an ordinary word

         