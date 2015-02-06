#lang planet neil/sicp

;Exercise 4.42
#|
1 2 3 4 5
  k b                     
e j              
    j   e
  k   m
b     m
|#
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

(define (exclusive-or x y)
  (and (or x y) (not (and x y))))
  
(define (puzzle-predicate? answers)
  (let ((betty (car answers))
        (ethel (cadr answers))
        (joan (caddr answers))
        (kitty (cadddr answers))
        (mary (cadr (cdddr answers))))
    (and (exclusive-or (= kitty 2) (= betty 3))
         (exclusive-or (= ethel 1) (= joan 2))
         (exclusive-or (= joan 3) (= ethel 5))
         (exclusive-or (= kitty 2) (= mary 4))
         (exclusive-or (= betty 1) (= mary 4)))))

(filter puzzle-predicate? (list-permutations (list 1 2 3 4 5)))

;kitty
;joan 
;betty
;mary 
;ethel
