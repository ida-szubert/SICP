#lang racket
;Concurrency

;concurrent programs are susceptible to subtle bugs connected with timing
;like tha fact that when person A checks the balance on an account, and attempts to withdraw some amount
;this attempt might prove invalid if person B withdraws money in between the operations called by A
;this happens because variables are shared between different processes
;and with concurrent processes we may not be able to control the order of assignment made by different processes

;a stringent restriction would be that no two operations that change a shared state variable can occur at the same time
;a less stringent one would ensure that a concurrent system produces the same result as if it was sequential

;Exercise 3.38
;initial balance: 100
;the following operations happen concurrently
;Peter: (set! balance (+ balance 10))
;Paul: (set! balance (- balance 20))
;Mary: (set! balance (- balance (/ balance 2)))

;a.
;assuming that they are forced to run sequentially:
;1. (100+10-20)/2 = 45
;2. (100+10)/2 - 20 = 35
;3. (100-20)/2 + 10 = 50
;4. 100/2 +10-20 = 40

;b. assuming they are allowed to be interleaved
;5. D10, W20 in the meantime (=80), take half = 40
;6. W20, D10 in the meantime (=110), take half = 55
;7. D10, take falf in the meantime (=50), W20 = 30
;8. W20, take half in the meantime (=50), D10 = 60
;9. take half, D10 in the meantime (=110), W20 = 90
;10. take half, W20 in the meantime (=80), D10 = 90


;Exercise 3.39
;this serialization means that the execution of p2 and of (lambda () (* x x)) cannot be interleaved
;which means that p2 cannot reset x in between the two times (lambda ()...) accesses x
;we're left with 100, 121, 101


;Exercise 3.40
;when procedures are parallel
;1. p1, p2: 1000000 (first set x to x^2, then set x to x^3)
;2. p2, p1: 1000000 (first set x to x^3, then set x to x^2)
;3. p1 reads, p2 reads and sets, p1 sets: 100
;4. p2 reads, p1 reads and sets, p2 sets: 1000
;5. p1 reads, p2 reads and sets, p1 reads, p1 sets: 10*1000 = 10000
;6. p2 reads, p1 sets, p2 reads, p2 reads, pt sets: 10*100*100 = 100000
;7. p2 reads, p2 reads, p1 sets, p2 reads: 10*10*100 = 10000

;when procedures are serialized
;only 1000000 (options 1 and 2)


;Exercise 3.41
;this is a bank-account procedure secured from parallel changes in balance:
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

;this way, two processes cannot access one balance concurrently
;each account has its own serializer, so processes on different account can proceed concurrently
;Is it neccessary to serialize the balance procedure as well?
;it doesn't seem so
;asking for balance does not alter it in any way, so it shouldn't mess with the result of other procedures
;the only possible reason to do that is tha we may be checking the balance in the middle of some operations taking place
;but it isn't a problem really

;Exercise 3.42
;what about doing the calls to protected outside the dispatch procedure
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) protected-withdraw)
            ((eq? m 'deposit) protected-deposit)
            ((eq? m 'balance) balance)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch)))
  
;I don't really see a difference between using "the same" serialized procedure whenever withdraw/deposit is called
;and creating a new serialized procedure in response to every withdraw or deposit call



  