#lang racket

;Exercise 4.74
;in negate, lisp-value, and find-assertions the procedure that is mapped over the frame streamalways produces either the empty stream or a singleton stream
;no interleaving is needed when combining such streams
(define (negate operands frame-stream)
  (stream-flatmap (lambda (frame)
                    (if (stream-null? (qeval (negated-query operands) (singleton-stream frame)))
                        (singleton-stream frame)
                        the-empty-stream))
                  frame-stream))

(define (lisp-value call frame-stream)
  (stream-flatmap (lambda (frame)
                    (if (execute (instantiate call frame (lambda (v f) ;execute applies predicate to arguments
                                                           (error "Unknown pat var: LISP-VALUE" v))))
                        (singleton-stream frame)
                        the-empty-stream))
                  frame-stream))

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum) (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

;stream-flatmap falttens the result of mapping a procedure over a stream
;in case of negate and lisp-value it's a stream of frames, and the procedure either returns the input frame, or the empty string
;in case of find-assertions a mapping is over a stream of assertions, and either a stream with a matching assertion is returned, or the empty stream
;each stream-map function outputs a stream of singleton and empty streams
;flattening is done so that all elements or singleton streams are collected into one stream, and the empty streams are ignored
;normaly when flattening streams it's good to interleave, i.e. extract elements from a number streams interchangibly, rather than extract everything from stream 1, then from stream 2...
;because if streams are infinite, we'll never see the elements of stream 2 in the output stream of the flattening procedure

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed (stream-car stream)
                          (delay (flatten-stream (stream-cdr stream))))))

;but in case of the functions written above, we know that we're dealing with very small streams, and that there's no danger in appending rather than interleaving
;a.
(define (simple-stream-flatmap proc s)
  (simple-faltten (stream-map proc s)))
(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (x) (not (stream-null? x)))
                             stream)))

;b
;Does the system's behavior change if simple-stream-flatmap is used instead of stream-flatmap in negate, lisp-value, and find-assertions?
;I general no, but there would be a problem if the input to simple-flatten was infinite
;but that would only happen in a database with an infinite number of assertions
;Anyway, simple-flatten would try to evaluate stream-filter before mapping stream-car, but filtering would never stop
;Unless of course stream-filter itself operates on delayed stream
;Then there should be no difference in operation between the normal and simple version of stream-flatmap
