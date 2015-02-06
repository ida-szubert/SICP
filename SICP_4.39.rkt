#lang racket

;Exercise 4.39
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (copper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (distinct? (list baker copper miller fletcher smith)))
    (require (not (= baker 5)))
    (require (not (= copper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller copper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher copper)) 1)))
    (list (list 'baker baker) (list 'copper copper)
          (list 'miller miller) (list 'fletcher fletcher)
          (list 'smith smith))))
;Does the ordering of requirements matter?
;For starters, it would be faster if instead of writing requirements we would simplylimit the arguments of amb in the simple cases of "does not live on nth floor"

;when checking distinct? and getting false, we know neither what is the reoccuring number nor to what variables is it connected
;so we change smith, ether though he might not be a problem
;we always change smith when sth goes wrong, no matter what is it
;so maybe it'd make sense to start with restictions having to do with smith
;so that if requirement is not met, the point we backtrack to can really have an influence on whether on the next try the requirement will be met
;then requirements pertaining to fletcher, miller, copper and baker should follow

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (copper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (not (= (abs (- fletcher copper)) 1)))
    (require (not (= copper 1)))
    (require (> miller copper))
    (require (not (= baker 5)))
    (require (distinct? (list baker copper miller fletcher smith)))
    (list (list 'baker baker) (list 'copper copper)
          (list 'miller miller) (list 'fletcher fletcher)
          (list 'smith smith))))

;Even if it doesn't really affect efficiency much (and I cannot check now, without a working interpreter)
;it is certainly a good idea to have distinct? requirement last, since it takes the most time to compute this predicate
