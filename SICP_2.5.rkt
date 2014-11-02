#lang racket

(define cons-integers
  (lambda (a b)
    (* (expt 2 a) (expt 3 b))))

;e.g the pair 4 2 would be represented as
;2^4 * 3^2 = 16 * 9 = 144
;2 * 2 * 2 * 2 * 3 * 3

;take x, divide it by 2 as long as it's possible without remainder
;keep count of how many time it was done
;and then divide by 3 untill you reach 1 and keep count how many times it was done
;144
;72 -> 1
;36 -> 2
;18 -> 3
;9 -> 4
;3 -> 1
;1 -> 2

(define (car x)
  (define (count-2s x counter)
    (if (= (remainder x 2) 0)
        (count-2s (/ x 2) (+ counter 1))
        counter))
  (count-2s x 0))

(define (cdr x)
  (define (count-3s x counter)
    (if (= (remainder x 3) 0)
        (count-3s (/ x 3) (+ counter 1))
        counter))
  (count-3s x 0))


(car 144)
(cdr 144)
                  