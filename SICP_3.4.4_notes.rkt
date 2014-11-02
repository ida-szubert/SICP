#lang racket

;A simulator for digital circuits
;digital logic simulations
;kind of an event-driven simulation
;actions (aka events) trigger futher events that happen at a later time, and these in turn trigger other events etc

;we will used objects that correspond to elementary circuit components

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (inverter-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input inverter-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;Exercise 3.29
;or-gate can also be build out of and-gates and inverters

(define (another-or-gate a1 a2 output)
  (let ((b (make-wire)) (c (make-wire)) (d (make-wire)) (e (make-wire)) (f (make-wire)))
    (first-and (and-gate a1 a1 b))
    (second-and (and-gate a2 a2 d))
    (third-and (and-gate c d f))
    (first-invert (inverter b c))
    (second-invert (inverter d c))
    (third-invert (inverter f output))
    'ok))

;this or-gate has delay time = 3xand-delay + 3xinverter-delay


;Exercise 3.30
(define (ripple-adder a-list b-list s-list c)
  (let ((cn c)
        (cn+1 (make-wire)))
    (define (help a-list b-list c-in s-list c-out)  
      (if (null? a-list)
          'done
          (begin (full-adder (car a-list) (car b-list) c-in (car s-list) c-out)
                 (set! cn cn+1)
                 (help (cdr a-list) (cdr b-list) cn (cdr s-list) cn+1))))
    (help a-list b-list cn s-list cn+1)))

          
