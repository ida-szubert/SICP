#lang racket

;Exercise 4.76
;implement speciaf form called unique
;(unique <query>)
;returns a singleton stream if only one item in the database satisfies the query
;and an empty stream if none or more than one does

(define (uniquely-asserted pattern frame-stream)
  (stream-flatmap (lambda (frame)
                    (let ((extension (qeval pattern (singleton-stream frame))))
                      (if (singleton-stream? extension)
                          extension
                          the-empty-stream)))
                  frame-stream))

(define (singleton-stream? stream)
  (and (not (stream-null? stream))
       (stream-null? (stream-cdr stream))))


;Exercise 4.76
;the implementation of and is elegant, but not efficient
;the reason is that when evaluating the second conjunt we need to inspect assertions from the database for each frame handed down by the first conjunct
;if the database has n elements, and a typical query outputs n/k frames, we need to perfonm n^2/k pattern matches
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame-stream))))
;we could also process the two clauses of an and separately and look for pairs of output frames that are compatible
;this would mean performing only n^2/k^2 pattern matches

(define (conjoin conjuncts frame-stream)
  (let ((stream1 (qeval (first-conjunct conjuncts) frame-stream))
        (stream2 (qeval (stream-car (rest-conjunct conjuncts)) frame-stream)))
    (let ((pairs (stream-permutations stream1 stream2)))
      (conjoin  (cdr (rest-conjuncts conjuncts))
                (stream-map (lambda (x) (combine-if-possible (car x) (cadr x)) pairs))))))

(define (combine-if-possible frame1 frame2)
  (define (iter variables)
    (cond ((stream-null? variables) (combine frame1 frame2))
          ((eq? (get-value (stream-car variables) frame1)
                (get-value (stream-car variables) frame2))
           (iter (stream-cdr variables)))
          (else 'fail)))
  (define (combine f1 f2)
    (let ((variables2 (variables f2))
          (values2 (values f2)))
      (define (iter-combine f vars vals)
        (if (stream-null? vars)
            f
            (iter-combine (extend-if-possible f (stream-car vars) (stream-car vals))
                          (stream-cdr vars)
                          (stream-cdr vals))))
      (iter-combine f1 variables2 values2)))
  (let ((shared-variables (get-repeating (variables frame1) (variables frame2))))
    (iter shared-variables)))
;it takes all the variables which are bound both in frame1 and frame2
;iter goes through them checking if the values bound to these variables are the same in both frames
;if so, frames are combined
;combinig proceeds by extending one frame with the bindings taken from the second one
;it could be simpler, e.g.
(define (combine-frames f1 f2)
  (cons (append (variables frame1) (variables frame2))
        (append (values frame1) (values frame2))))
;but this would potentially result in a frame with repeating variables, which is bad
;extending a frame, on the other hand, adds a binding only if the variable is not already bound



;Exercise 4.77
(and (supervisor ?x ?y)
     (not (job ?x (computer programmer))))

(and (not (job ?x (computer programmer)))
     (supervisor ?x ?y))
;These should be the same - we're looking for all supervisors of non-programmers
;First query finds supervisors, and filters the resulting frame stream by removing all frames in which x's job is programmer
;Second query removes all frames in which x's job is programmer
;but wait, the input stream is the empty frame singleton stream, the empty frame gets filtered out, the second conjuncts gets an empty stream as input, and empty stream is returned
;in general, not works as we imagine it should when all variables used in it are bound in the input frames

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame-stream))))

(define (conjoin conjuncts frame-stream)
  (define (iter vars patterns frames)
    (if (contained? vars (variables (first-conjunct patterns)))
        (lambda (x) (conjoin (rest-conjuncts patterns) (qeval x (qeval (first-conjunct patterns) frames))))
        (iter vars (cdr patterns) (qeval (first-conjunct patterns) frames))))
  (if (null? conjuncts)
      frame-stream
      (let ((first (first-conjunct conjuncts))
            (rest (rest-conjuncts conjuncts)))
        (if (negation? first)
            ((iter (variables first) rest frame-stream) first)
            (conjoin rest (qeval first frame-stream))))))

(define (variables pattern)
  (define (iter l)
    (if (null? l)
        '()
        (if (var? (car l))
            (cons (cdar l) (iter (cdr l)))
            (iter (cdr l)))))
  (let ((expanded-pattern (query-syntax-process pattern)))
    (iter expanded-pattern)))

(define (contained? l1 l2)
  (cond ((null? l1) #t)
        ((member? (car l1) l2) (contained? (cdr l1) l2))
        (else #f)))
(define (member? x l)
  (cond ((null? l) #f)
        ((eq? x (car l)) #t)
        (else (member? x (cdr l)))))
(define (negation? exp)
  (eq? (car exp) 'not))
;WORKS
;disjunction would need to be similarily altered

            








