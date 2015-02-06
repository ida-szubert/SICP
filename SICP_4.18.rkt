#lang racket

;Exercise 4.18
(lambda <vars>
  (let ((u *unassigned*)
        (v *unassigned*))
    (set! u <exp1>)
    (set! v <exp2>)
    <exp3>))

(lambda <vars>
  (let ((u *unassigned*)
        (v *unassigned*))
    (let ((a <exp1>)
          (b <exp2>))
      (set! u a)
      (set! v b)
      <exp3>)))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;option 1:
(lambda (f y0 dt)
  (let ((y *unassigned*)
        (dy *unassigned*))
    (set! y (integral (delay dy) y0 dt)) ;here comes the problem- during assignment the value is evaluated
    (set! dy (stream-map f y))
    y))

;option 2:
(lambda (f y0 dt)
  (let ((y *unassigned*)
        (dy *unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))

;Option 2 will not work, because definition of dy refers to y, which has the value *unassigned* when dy needs to be evaluated