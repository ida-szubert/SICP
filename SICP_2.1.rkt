#lang racket

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;(define (gcd a b)
;  (if (zero? b)
;      a
;      (gcd b (remainder a b))))
  
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (* n -1) g) (/ (* d -1) g))
        (cons (/ n g) (/ d g)))))

(define c (make-rat -1 -2))
(define b (make-rat -1 2))
(define a (make-rat 1 -2))
(print-rat c)
(print-rat b)
(print-rat a)

;it works with the gcd which is build into racket
;it doesn't work with the gcd I was using earlier
;the reason is that gcd can come out negative, and this messes with the make-rat function