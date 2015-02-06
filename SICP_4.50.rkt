#lang racket

;Exercise 4.50
;ramb - like amb except it searches alternatives in a random order
;each time a ramb expression is evaluated, the choices get shuffled
;when they are accessed during next evaluation, the order will be different
;it doesn't need to be a complete shuffle- it enough it a random element of the list is moved to the first position
;if the order of choices is changing, the method of choosing can be exactly the same as in amb
;this needs to happen during evaluation, not analysis, otherwise the analysed ramb expression will have a fixed order of choices
;but the choices themselves can be analysed

((ramb? exp) (analyze-ramb exp))
(define (ramb? exp) (tagged-list? exp 'ramb))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env succeed (lambda () try-next (cdr choices))))))
      (try-next (shuffle cprocs))))
;i'm not sure if shuffle should happen within try-next, in the fail continuation, or when analyze-ramb calls try-next
;probably the latter, otherwise the first choice would not be random

(define (shuffle list)
  (let ((length (length list)))
    (let ((random-element (list-ref (random length) list)))
      (cons random-element (remove random-element list)))))

