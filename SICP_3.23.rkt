#lang planet neil/sicp
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (make-deque)
  (cons '() '()))
(define (empty-deque? deque)
  (and (eq? (front-ptr deque) '()) (eq? (rear-ptr deque) '())))

(define (front-deque deque) (car (front-ptr deque)))
(define (rear-deque deque) (car (rear-ptr deque)))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-cdr! new-pair (front-pointer deque))
           (set-front-pointer! deque new-pair)))))

(define (rear-insert-deque! dequeu item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-cdr! (rear-pointer deque) new-pair)
           (set-rear-ptr! deque new-pair)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque"))
        (else (set-front-ptr deque (cdr front-ptr deque)))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque"))
        (else (set rear-ptr deque (????; that's tricky)
           
          
      
   