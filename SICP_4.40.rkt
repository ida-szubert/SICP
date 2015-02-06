#lang racket

;Exercise 4.40
;3125 vs 120

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (let ((fletcher (amb 1 2 3 4 5)))
      (require (not (= fletcher 1)))
      (require (not (= fletcher 5)))
      (let (smith (amb 1 2 3 4 5))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((copper (amb 1 2 3 4 5)))
          (require (not (= copper 1)))
          (require (not (= (abs (- fletcher copper)) 1)))
          (let ((miller (amb 1 2 3 4 5)))
            (require (> miller copper))
            (require (distinct? (list baker copper miller fletcher smith)))
            (list (list 'baker baker) (list 'copper copper)
                  (list 'miller miller) (list 'fletcher fletcher)
                  (list 'smith smith))))))))

