#lang planet neil/sicp
;Implementing serializers

;mutex- an object that supports two operations. It can be acquired, and it can be released
;once it has been acquired, no other operation on that mutex can proceed untill it is released
;each serializer has an associated mutex- thet's why processes with the same serializer cannot occur concurrently
;given a procedure p, serializer returns a procedure which acquires the mutex, runs p, and releases the mutex

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

;mutex is a mutable object that can hold the value true or false
;when #f, mutex is available; when #t it is already acquired

(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire) (if (test-and-set! cell)
                                  (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell #f))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))
;tests the cell and returns the result
;if the result is #f, it also sets it to #t
;but there's an importaint caveat when considering concurrent processes
;when a process tests the cell and finds it #f, the cell concents need to be set to #t before any other process can test the cell
;it means that the test-and-set process must be run atomically
;the actuall implementation of test-and-set depends on how the system runs concurrent processes
;e.g. it might use a time-slicing mechanism that cycles between processes
;test-and-set can work by disabling time slicing during the testing and setting

;Exercise 3.46
;drawing

;Exercise 3.47
;a semaphore of size n is a generalization of a mutex
;up to n processes can acquire a mutex concurrently
;a. implementation of a semaphor in terms of mutex

(define (make-semaphor n)
  (let ((counter 1)
        (mutex (make-mutex2)))
    (define (the-semaphor m)
      (cond ((eq? m 'acquire) (cond ((< counter n) (begin (set! counter (+1 counter))
                                                          (mutex 'acquire)
                                                          (set! (mutex 'cell) #f))) ;;mutex cell would need to be exposed for that
                                    ((= counter n) (begin (set! counter (+1 counter))
                                                          (mutex 'acquire))) ;;when we have reached the limit of processes, the mutex's cell should stay set to #t
                                                                                 ;;although I'm not sure, since the mutex is guarded by semaphor now, and it takes care of access
                                    (else (the-semaphor m))))
            ((eq? m 'release) (begin (set! counter (- counter 1))
                                     (the-mutex 'release)))))
    the-semaphor))

(define (make-mutex2)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire) (if (test-and-set! cell)
                                  (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))
            ((eq? m 'cell) cell)))
    the-mutex))
;if the command is 'acquire
;we check if there are less than n active processes
;if so, we increment the counter by 1
;tell the mutex a process wants to acquire it
;the mutex checks if the cell is set to #f
;it should be
;the process acquires the mutex, and we set the cell to #f again

;implemantation in terms of atomic test-and-set! operations

(define (make-semaphor-atomic n)
  (let ((counter 1)
        (cell (list #f)))
    (define (the-semaphor m)
      (cond ((eq? m 'acquire) (if (= counter n)
                                  (begin (test-and-set! cell) (set! counter (+ counter 1)))
                                  (if (< counter n)
                                      (begin (test-and-set! cell) (set! cell #f) (set! counter (+ counter 1)))
                                      (the-semaphor 'acquire))))
            ((eq? m 'release) (begin (set! cell #f) (set! counter (- counter 1))))))
    the-semaphor))


        
        
        
