#lang racket
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;1.6180327868852458
;phi is the fixed point of this function
;that's because phi is a number that fulfills the following equation
;x^2 = x + 1
;x = (x + 1) / x
;x = (x / x) + (1 / x)
;x = 1 + (1 / x) 