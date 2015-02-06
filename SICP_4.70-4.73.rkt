#lang racket

;Exercise 4.70
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS (cons-stream assertion old-assertions))
    'ok))
(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

;what's the purpose of the let bindings?
;THE-ASSERTIONS / THE-RULES are treated like mutable variables, which always store the current set of assertions and rules
;adding new assertion/rule means changing these variables
;so we need to see what's their current value, so that when constructing their new value we can use the current one, and simply cons the new element onto it
;The following would not work:
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS (cons-stream assertion THE-ASSERTIONS))
  'ok)
;The reason being that evaluation will go into a loop.
;THE-ASSERTIONS is a delayed argument, and it will be evaluated only when needed.
;Which means that when it's evaluated for the purpose of cons, it'll refer to the expression we're just in the middle of evaluating


;Exercise 4.71
;simple-query:
(define (simple-query query-pattern frame-stream)
  (strema-flatmap (lambda (frame) (stream-append-delayed (find-assertions query-pattern frame)
                                                         (delay (apply-rules query-pattern frame))))
                  frame-stream))

(define (simple-query-2 query-pattern frame-stream)
  (stream-flatmap (lambda (frame) (stream-append (find-assertions query-pattern frame)
                                                 (apply-rules query-pattern frame)))
                  frame-stream))

;disjoin:
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed (qeval (first-disjunct disjuncts) frame-stream)
                          (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))

(define (disjoin-2 disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave (qeval (first-disjunct disjuncts) frame-stream)
                  (disjoin (rest-disjuncts disjuncts) frame-stream))))

;Why use the -delay versions of append and interleave (which both need their second argument delayed)?
;The text states that "this postpones looping in some cases".
;What I see is that the second argument is not evaluated untill needed. In the case of simple-query this means that we find assertions
;matching the query, and if we need to, we can perform the more complicated matching involving rules.
;Using undelayed version of append means that we need to get all matching frames at once.
;Ther was one loop discussed in the text, considering the rule stating that married is a bidirectional predicate
;with delay, evaluation would stop, and asking for the rest of the stream would return the right answer again
;which is repetitive, but better than neverending evaluation



;Exercise 4.72
;Why do disjoin and stream-flatmap interleave the streams rather than append them?
;Because if disjoint queries output infinite streams of frames, using append would mean that we only ever see the output of one of the queries.
;Same goes for stream-flatmap. If a function is mapped over a number of infinite streams, and we wanted to go though the in turn
;and make them into one stream, it would be effectively identical with the first stream


;Exercise 4.73
;Why does flatten-stream use delay explicitly?
;What's wrong with this:
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave (stream-car stream)
                  (flatten-stream (stream-cdr stream)))))

;flatten-stream is a function whose purpose is to take a stream whose elements might be other stream
;and flatten it
;

