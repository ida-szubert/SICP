#lang planet neil/sicp

;Exercise 4.43

#|
Moore             Lorna     Mary Ann
Downing           Melissa   ?Lorna?
Hall              Rosalind  ?Gabrielle?
Barnacle Hood     Gabrielle Melissa
Parker            Mary Ann  (not Gabrielle) ?Rosalind?

Lorna is Downing's daughter
|#

(define (daughters-and-yachts)
  (let ((m-d 'Mary-Ann)
        (b-d 'Mellisa)
        (m-y 'Lorna)
        (d-y 'Mellisa)
        (h-y 'Rosalind)
        (b-y 'Gabrielle)
        (p-y 'Mary-Ann)
        (p-d (amb Lorna Mellisa Rosalind Gabrielle Mary-Ann)))
    (require (not (eq? p-d 'Gabrielle)))
    (require (or (eq? p-d h-y) (eq? p-d d-y)))
    (let ((d-d (amb Mellisa Rosalind Gabrielle Mary-Ann))
          (h-d (amb Lorna Mellisa Rosalind Gabrielle Mary-Ann)))
      (let ((daughters (list m-d d-d h-d b-d p-d))
            (yachts (list m-y d-y h-y b-y p-y)))
        (require (eq? (list-ref (find-position p-d yachts) daughters)
                      'Gabrielle))
        (require (not (eqlist? daughters yachts)))))))

(define (find-position x list)
  (cond ((eq? (car list) x) 0)
        (else (+ 1 (find-position x (cdr list))))))

            
        