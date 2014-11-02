#lang racket

;abstraction by combining data objects - forming compound data
;data abstraction - isolating the parts of a program that deal with how data objects are represented (i.e. constructed from more primitive data objects)
;                   from the parts dealing with how data objects are used


;(define linear-combination
 ; (lambda (a b x y)
  ;  (+ (* a x) (* b y))))

;we need not think only about numbers
;linear-multiplication could be used for whatever type of data which has addition and multiplication defined
;(define linear-combination
 ; (lambda (a b x y)
  ;  (add (mul a x) (mul b y))))


;Arithemtics for rational numbers
;if we had these procedures:
;(make-rat <n> <d>) ; - constructing rational numbers from the numerator and the denominator
;(numer <x>) ; - extracting the numerator
;(denom <x>) ; - extracting the denominator
;we could easily define the following:
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;we can use cons to create pairs (and lists in general)
; and use car and cdr to access elements of those pairs and lists
;when constructing a rational number we want it to be reduced to the lowest terms
(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(define  one-third (make-rat 1 3))
(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

