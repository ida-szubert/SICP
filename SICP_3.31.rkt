#lang planet neil/sicp

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))

;Agenda
;keeps track of what's supposed to happen

(define (after-delay delay action)
  (add-to-agenda (+ delay (current-time the-agenda))
                 action
                 the-agenda))

;propagate is the procedure which drives the simulation
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;placing probe on a wire
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input1 (make-wire))
(define input2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

;Exercise 3.31
;there's one non-obvious part of the make-wire procedure
;why, when adding a procedure to action-procedures, we run it immediately?

;let's say we've got a half-adder
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(half-adder input1 input2 sum carry)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;once again, what happens to the wires when we buildan and-gate?
;a1 and a2 get and-action-procedure
;they cons it onto their action-procedures list
;and run it immediately
;which entails calling after-delay
;which adds the and-action-procedure to the agenda

;now, we set a1 to 1
;which means that we call-each action-procedures
;i.e. we call and-action-procedure
;now, I have no idea how is it accessed, since it's an internal procedure of and-gate

;when we call propagate, the function (lambda () (set-signal! output new-value) is run
;if we didn't run the and-action-procedure immediately, it wouldn't get on the agenda
;and wouldn't be called by propagate

;BUT what for the action-procedures list internal to the wire?


;FIRST TRY
;what are the action procedures of the input1 wire?
;input1 is an input to an or-gate  and an and-gate 
;which means it's got an and-action-procedure and an or-action-procedure on its list (in this order)
;the first one got there got there through add-action! when building the and-gate
;when building the gate, the and-gate-procedure was added to the list, and then run
;this means that the value of the output of the gate was set so that it corresponds with the input values
;the same was done when the second input wire accepted the and-gate-procedure
;What for?
;the procedures are run immediately when the circuit is being build. It means that before the simulation starts, the signals on the wires are
;coordinates, so to speak.
;now, when we set input1 to 1
;input2 is 0, the default when creating a wire
;and-gate-procedure is run, and carry is set to 0
;or-gate-procedure is run, and output is set to 1
;inverter output is set to 1
;sum is set to 1

;I don't see why is propagate necessary or where does agenda come in

;On the internet:
; The point is that if we don't call it in the point of time we add it to the list
; of actions we will not add it to agenda. In that case we will add
; it to the agenda only when we call (propagate) procedure which will produce
; different results. In fact if we don't do it, no procedures will be in the agenda at all, so
; our simulation will just not run.
  
  
  