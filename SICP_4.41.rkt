#lang racket

;Exercise 4.41
;one approach is to generate all possible permutations of a 5-element list without repetitions whose elements are values between 1 and 5
;make a list of them, and filter that list using the constraints

(define (reverse pair)
  (list (cadr pair) (car pair)))

(define (rember l x)
  (if (null? l)
      '()
       (if (eq? (car l) x)
           (cdr l)
           (cons (car l) (rember (cdr l) x)))))

(define (list-permutations l)
  (define (help lst count)
    (if (= (length lst) 2)
        (list lst (reverse lst))
        (let ((first (car lst)))
          (if (< count (length l))
              (append (map (lambda (x) (cons first x)) (list-permutations (rember l first)))
                      (help (append (cdr lst) (list (car lst))) (+ count 1)))
              '()))))
  (help l 0))


(define (puzzle-predicate? answers)
  (let ((baker (car answers))
        (copper (cadr answers))
        (fletcher (caddr answers))
        (miller (cadddr answers))
        (smith (cadr (cdddr answers))))
    (and (not (= baker 5))
         (not (= copper 1))
         (not (= fletcher 5))
         (not (= fletcher 1))
         (> miller copper)
         (not (= (abs (- smith fletcher)) 1))
         (not (= (abs (- fletcher copper)) 1)))))
  
(filter puzzle-predicate? (list-permutations (list 1 2 3 4 5)))

  
  