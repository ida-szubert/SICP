#lang racket

;CHAPTER 3

;Object-based approach to structuring modular systems

;local state variables
(define balance 100)
(define (withdraw amount)
(if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))

;(set! <name> <new value>)
;right now, balance is accessible to any procedure
;we might want to make it internal to withdraw somehow
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                       balance)
          "Insufficient funds"))))
(new-withdraw 30)
(new-withdraw 20)
(new-withdraw 51)
(new-withdraw 18)

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
                     balance)
        "Insufficient funds")))
;make-withdraw is a procedure which produces withdraw procedures with a given initial balance
;withdraw procedures made using make-withdraw don't need the let clause, because formal paramiters (here: balance) are already local

(define W1 (make-withdraw 150))
(define W2 (make-withdraw 70))

(W1 50)
(W2 50)
(W1 25)
(W2 25)
(W1 10)
(W2 10)


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)
;the dispatch procedure is returned as the value that represents the account object.
;seems strange, but it's like the message-passing style seen earlier
;objects can be represented by procedures specyfing what happens when the object is involved in some kind of operation
;here, dispatch specifies what happens when we ask "withdraw" or "deposit" of the object

(define A (make-account 100))
((A 'withdraw) 40)
((A 'deposit) 100)

;Exercise 3.1
(define (make-accumulator sum)
  (define (add n)
    (set! sum (+ sum n))
    sum)
  add)

(define a (make-accumulator 5))
(a 10)
(a 10)

;Exercise 3.2
;it is sometimes useful to be able to count the number of tymes a given procedure is called during the course of computation

(define (make-monitored f)
  (let ((counter 0))
    (define (mf input)
      (if (eq? input 'how-many-calls)
          counter
          (begin (set! counter (+ 1 counter))
                 (f input))))
  mf))
    
(define (square x) (* x x))
(define s (make-monitored square))
(s 10)
(s 'how-many-calls)

;Exercise 3.3
(define (make-account-with-password balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p  m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m)))
        (lambda (x) (cond (else "Incorrect password"))))) ;;after I've changed this clause to contain a function, make-account-with-password works
    dispatch)

(define A1 (make-account-with-password 200 'cheesecake))
((A1 'cheesecake 'deposit) 20)
((A1 'cheesecake 'withdraw) 80)
((A1 'poundcake 'withdraw) 40)
;I don't quite get why it results in an error message from dr Racket, rather than simply printing my error message
;another version attempted to deal with the fact that (A1 'poundcake 'withdraw does not yield a procedure that could be applied to 40

(define (make-account-with-password2 balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (error-wrong-password amount)
    (error ("Incorrect password"))) 
  (define (dispatch p  m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m)))
        error-wrong-password))
    dispatch)


(define A2 (make-account-with-password2 200 'cheesecake))
((A2 'cheesecake 'deposit) 20)
((A2 'cheesecake 'withdraw) 80)
;((A2 'poundcake 'withdraw) 40)
;but it doesn't work either

;a third take
(define (make-account-with-password3 balance password)
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (wrong-password amount)
    (cond (else "Incorrect password")))
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request:" m)))
        wrong-password))
  dispatch)

(define A3 (make-account-with-password3 200 'cheesecake))
((A3 'cheesecake 'deposit) 20)
((A3 'poundcake 'withdraw) 20) 
;and this time it works                                     

;Exercise 3.4
(define (make-account-counted-trials balance password)
  (let ((failure-count 0))
    (define (deposit amount)
      (begin (set! balance (+ balance amount))
             balance))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (wrong-password amount)
      (cond (else "Incorrect password")))
    (define (call-the-cops amount)
      (cond (else "Wrong password entered on 8 consequtive trials. Notifying authorities.")))
    (define (dispatch p m)
      (if (eq? p password)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request:" m)))
          (if (eq? failure-count 7)
              call-the-cops
              (begin (set! failure-count (+ failure-count 1))
                     wrong-password))))
    dispatch))

(define A4 (make-account-counted-trials 200 'cloak))
((A4 'cloek 'withdraw) 20)
((A4 'claek 'withdraw) 20)
((A4 'cloac 'withdraw) 20)
((A4 'kloak 'withdraw) 20)
((A4 'ploak 'withdraw) 20)
((A4 'cluak 'withdraw) 20)
((A4 'cjoak 'withdraw) 20)
((A4 'clowk 'withdraw) 20)
;i've tried making failure-count internal to dispatch, but it didn't work
;it was as if on each trial failure-count was 0
;it works if it's external to dispatch