#lang racket

;Exercise 4.61
(rule (?x next-to ?y in (?x ?y . ?u)))
(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))

(1 next-to (2 3))
((2 3) next-to 4)

(2 next-to 1)
(3 next-to 1)

;Exercise 4.62
(rule (last-pair (?x) (?x)))
(rule (last-pair (?x . ?y))
      (last-pair (?y)))

;Exercise 4.63
(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mahujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)

(rule (grandson ?G ?S)
      (son ?G ?F)
      (son ?F ?S))


(rule (son ?D ?S)
      (or (son ?D ?S)
          (and (son ?W ?S)
               (wife ?D ?W))))