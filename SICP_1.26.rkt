#lang racket
;Exercise 1.26

(define expmod
  (lambda (base exp m)
    (cond ((zero? exp) 1)
          ((even? exp) (remainder (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m)))))

;This differs from the original definition in that instead of
;(remainder (square (expmod base (/exp 2) m)) m)
;it uses
;(remainder (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m)) m)
;This means that in when exp is even, the original algorithm creates only one recursive call, an computes the result only once
;The new algorithm in every such case creates two recursive calls that must be evaluated, so there's massove redundancy
;It's not the case that there will be twice as many expressions to evaluate overall
;there will be many more
;Let's think about the first call to expmod
;Let's say exp is even
;We create two expressions to be evaluated, instead of one that would be created in the original version
;let's evaluate them
;let's say new exp is also even
;again, we generate 2 expressions instead of one
;and this happens in both evaluations
;by this point rather than evaluating expmod 2 times, we need to evaluate it 4 times
;by the next step, it'll be 3 compared to 8
;the original process was O(log n)
;now we generate 2^n steps, and since (log 2^n) simplifies to n, the process is O(n)