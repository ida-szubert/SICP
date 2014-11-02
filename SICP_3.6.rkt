#lang planet neil/sicp
;Exercise 3.6

(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a x) c) m)))
(define random-init 137)

(define (rand n)
  (let ((x random-init))
    (cond ((eq? n 'generate) (set! x (rand-update x)))
          ((eq? n 'reset) (lambda (y) (set! x y)))))) 

(define generate-random (rand 'generate))
(define reset-random x ((rand 'reset) x))
                          
(define old-rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
                          
;but there's no implementation of random-init and rand-update, specifically what are the "appropriately chosen integers"
;i cannot test :(

