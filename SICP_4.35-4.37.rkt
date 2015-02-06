#lang racket

;Exercise 4.35

(define (an-integer-between low high)
  (amb low high (an-integer-between (+ low 1) (- high 1))))

(define (pythagorean-triple-between low high)
  (let ((i (an-integer-betwen low high)))
    (let ((j (an-integer-betwen i high)))
      (let ((k (an-integer-betwen j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))


;Exercise 4.36
;We want to write a function which will, in principle, generate all Pitagorean triples
;Why not just replace all an-integer-between instances above with an-integer-starting-from?
;That wouldn't work because we would never backtrack futher than k
;having chosen an i and an j, we will forever try backtrack to k in serch of a third number to comple a Pitagorean triple
;but there might not be such a number, given the chosen i and j

;first try
(define (all-pythagorean-triples)
  (define (find-pythagorean-triples low high)
    (amb (pythagorean-triple-between low high)
         (find-pythagorean-triples low (+ high 1))))
  (find-pythagorean-triples 0 100))
;it could possibly be ok if find-pythagorean-triples memorized its results somewhere, so that upon finding a triple it would check if it was already reported

;second try
(define (all-pythagorean-triples)
  (let ((k (an-integer-starting-from 0)))
    (let ((j (an-integer-between 0 k)))
      (let ((i (an-integer-between 0 j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
  

;Exercise 4.37
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require ((>= hsq ksq)))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

;Is this more efficient than 4.35?
;let's say between 1 and 10
;we choose i and j such that i =< j and the sum of squares is lower the the square of the highes boundary
;which makes sense, since there's no point looking for a matching triple if the sum of squares of two elements is greater then the square of highest possible third number
;"Consider the number of possibilities that must be explored" - to get an answer, I take it, not all answers

;original version:
;choose i, choose j, choose k, if it doesn't work keep trying different ks
;if this doesn't work it means you've explored (- high low) possibilities
;keep i, choose different j, go though ks
;i.e. every time you pick new j you've tested a whole range of ks
;every time you pick a new i you've testes a whole range of js, i.e. range^2 tries
;so, to get to (3 4 5), when low=1, it takes range^2 + range^2 + 4*range + 5
;if range is 1-10, 100+100+40+5=245

;new version:
;choose i, choose j, check if their sum of squares is smaller than hsq
;if not, choose another j
;if you find a good pair, check if srt of sum of squares is an integer
;to get to (3 4 5)
;3*10+4=34

;New version is much more efficient
;it doesn't enumerate through the third element of a triple at all