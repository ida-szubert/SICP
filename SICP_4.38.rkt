#lang racket

;Exercise 4.38
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (copper (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (distinct? (list baker copper miller fletcher smith)))
    (require (not (= baker 5)))
    (require (not (= copper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller copper))
    (require (not (= (abs (- fletcher copper)) 1)))
    (list (list 'baker baker) (list 'copper copper)
          (list 'miller miller) (list 'fletcher fletcher)
          (list 'smith smith))))

baker      1 2 3 4
copper     2 3 4 5 --> 4 / 5 / 5 / 2 / 2
fletcher   2 3 4   --> 2 / 2 / 3 / 4 / 4
miller     2 3 4 5 --> 5 / - / - / 3 / 5
smith      1 2 3 4 5

baker      1 / 3 / 1 / 1 / 3
copper     4 / 4 / 2 / 2 / 2
fletcher   2 / 2 / 4 / 4 / 4
miller     5 / 5 / 3 / 5 / 5
smith      3 / 1 / 5 / 3 / 1

;There are 5 possible solutions
;with the requirement that Smith's floor is not adjacent to Fletcher's, only the 5th one fits