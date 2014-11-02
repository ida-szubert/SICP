#lang racket

;Deadlock
;account exchage still has a problem
;Peter tries to exchange a1 and a2
;Paul tries to exchange a2 and a1
;Peter's process has entered a serialized procedure protecting a1
;the same goes for Paul and a2
;Peter cannot proceed, because he would need to enter a2's serialization
;the same holds for Paul
;that a deadlock
;How to avoid it?

;each account can have a unique identification number
;serialized-axchange needs to be rewriten so that a process will always attempt to enter a serialization of the lowest-numbered account first
;techniques of avoiding deadlocks are problem-specific

;Exercise 3.48
;Using numbered accounts helps to avoid the deadlock problem because in the situation described above both exchange processes will try to access
;accounts in the same order, let's say first a1, then a2
;rather than starting with the account which was given as the first argument of account-exchange
;We've made sure that two processes cannot access one account at the same time.
;Peter accesses a1, and when Paul attempts the same, he is denied and has to wait
;then peter retires a1, Paul accesses it, and Peter goes on to acces a2, which is free

;Exercise 3.49
;there's a shared reseource X that specifies the order of accessing other resources
;process 1 accessed X to see what aother shared resource to access next, let's say b
;moved to b
;process 2 then accessed X, and wanted to moe to b
;at which moment process 1 wanted to access X again, to learn where to move next

;Concurency, time, and communication
;deep problems with concurrency
;it's not always clear what's meant by shared state
;in multiprocessing systems the serializer approach is inefficient
;because of optimisation techniques (pipelining, cached memory) the contents of memory may not be in a consistent state at every instant
;and test-and-set! examine a global shared flag at arbitrary times
;What about large, distributed systems?
;e.g each branch maintains local values for balances, and periodically these are synchronized between branches
;when does balance change- when its changed in the local branch, or after synchronisation?

;any notion of time in concurrency control must be intimately tied to communication