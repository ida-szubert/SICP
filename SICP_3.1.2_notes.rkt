#lang planet neil/sicp
;introducing variables with changing values has many disadvantages
;one of them being having to let go the substitution model of evaluation
;and having to think about what the local environment of a function looks like when the function is running
;but, there are also some advantages

;generating random numbers:
;a reasonable description of what we expect is 
;that successive calls to rand will produce a sequence which has statistical properties of uniform distribution

;let's assume we have a procedure rand-update
;if you start with x1 and define x2 as (rand-update x1), and so on,
;such that te resulting sequence will resemble random distribution
;rand can be implemented as a procedure with local state variable x, whose initial value is assigned by random-init
;each call to rand computers rand-update and stores a new value of x

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define random-init 1)

;the same could be accomplished without assignment
;but then every part of the program which uses random numbers would need to remember the current value of x to pass it into rand-update the next time
;that's a major annoyance
;consider Monte Carlo simulation:
;we want an approximation of pi
;it is known that 6/pi^2 is the probability that two integers chosen at random will have no factors in common, i.e their gcd will be 1
;so, we perform a large number of experiments, choosing 2 integers at random and calculationg gcd
;procedure monte-carlo runs the experiment a specified number of times, and returns a fraction of the trials in which the result was true

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  iter trials 0)

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

;now without assignment:
(define (estimate-pi2 trials)
  (sqrt (/ 6 (random-gcd-test trials randm-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0) (/ trials-passed trials))
              ((= (gcd x1 x2) 1) (iter (- trials-remaining 1) (+ trials-passed 1)))
              (else (iter (-trials-remaining 1) trials-passed x2))))))
  (iter trials 0 initial-x))

;what's wrong with this version?
;it's not really modulra
;before we had a separate monte-carlo procedure, which could work with an arbitrary experiment procedure
;here, random-gcd-test handles explicilty two numbers
;but if we had a test with tree or four, we'd need to make substantial changes
;the Monte Carlo idea is just not clear in tis implementation



  
 

