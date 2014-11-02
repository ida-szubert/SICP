#lang racket

;serializers are a powerful abstraction
;using them is straightforward wen there's one shared resource

;?? but even if we serialize, we cnanot decide on the order of operations
;?? I know it wasn't the main issue of this section, but it remains a problem 

;let's say we want to swap the balances in two accounts
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance) (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;this works when only a single process it trying to do the exchange
;Peter exchanges a1 and a2, while Paul exchanges a1 and a3
;each account's withdrawals and deposits are serialized
;it doesn't solve the problem, because Paul might do the exchange in between Peter's calculating the difference and performing the operations
;what is needed is that exchange procedure locks out any other concurrent accesses to the accounts
;we can use the serializers of both accounts to serialize the whole exchange procedure
;to do that, we need to expose the serialzer, which is now internal to the account procedure

(define (make-account-and-serializer balance)
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
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) protected deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

;now it is the responsibility of each user to explicitly manage serialization
(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))


;Exercise 3.43
;a1 10
;a2 20
;a3 30
;if the processes are run sequentially, after any number of concurrent exchanges the balances will still be 10, 20, and 30, in one order or another
;if exchanges are rune one after another, each exchange operation preserves the balances
;i.e. individual balances are changed, but in such a way that the final amount of money on the accounts are the same as they were at the start
;only that a1 has the amount that a2 previously hold, and vice versa
;there's no way for the amounts to change

;What is processess are not run sequentially?
;then, let's say, one process would access balances of a1 and a2 and calculate the difference
;in the meantime, another process would do the same with a1 and a3
;first process withdraws 10 from a2, and deposits 10 in a1
;a1 20, a2 10
;second process then withdraws 20 from a3 and deposits 20 in a1
;a1 40, a3 10
;the total amount on all accounts is still 60, but it's distributed differently

;What if the processes on individual account were not serialized?
;for instance
;first process reads balance a3 and balance a1
;second process reads balance a3 and balance a2
;first process sets balance a3 to 10 and balance a1 to 30
;second process, working under the assumption that a3 is 30, sets a3 to 20 and a1 to 30
;whereas we would rather want it to set a3 to -10, starting from 10 and withdrawing 20
;in the end the total amount on all accounts is 80

;Exercise 3.44
;is there a problem in with this procedure for transferring money between accounts?
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

;this should work fine
;as have been analysed in 3.43, when the processes between accounts are not serialized, the total amount of money is conserved
;that's all we care about with transfers
;there's no in-between time, as there is between accessing balances&calculating difference and setting balances
;with transfers the amount is given, and we only access the individual account once
;with exchange, we need to make sure that in between calculating and setting the balances won't be changed by another process
;because we care about preserving the actual amounts as they were initialy

;Exercise 3.45
;How about going back to authomatically serializing withdrawals and deposits?
(define (make-account-and-serializer2 balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

;then deposits and withdrawals might be handled as earlier, without the user worrying about serialization

;What happens when serialized-exchange is called?
((balance-serializer1 (balance-serializer2 exchange)) a1 a2)
((balance-serializer1 (balance-serializer2 (a1 'withdraw x) (a2 'deposit x))))
((balance-serializer1 (balance-serializer2 ((balance-serializer withdraw) x) ((balance-serializer deposit) x))))
;depositing x in a2 and the whole exchange operation are now in one serializer
;effectively, so is withdrawal of x from a1 and the whole exchange
;it seems like the evaluation will never stop
;the exchange process has acquired the mutex, and it will attempt to acquire it again, which can't happen



 
                             
        