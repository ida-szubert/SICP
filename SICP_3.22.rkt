#lang planet neil/sicp

;Exercise 3.22
(define (make-queue2)
  (let ((front-ptr '())
        (rear-ptr '()))
    
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with and empty queue")
        (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
                (set-cdr! rear-ptr new-pair)
                (set! rear-ptr new-pair)
                front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called on the empty queue"))
            (else
              (set! front-ptr (cdr front-ptr))
              front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) front-ptr)
            (else (error "Operation not defined for queues:" m))))
    dispatch))

(define q2 (make-queue2))
((q2 'insert-queue!) 3)
(q2 'print-queue)