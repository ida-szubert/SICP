#lang racket

;Exercise 4.64
;instead of
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

;the rule is defined as follows:
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?staff-person ?middle-manager)
               (supervisor ?middle-manager ?boss))))

;what happens during evaluation of the query
(outranked-by (Bitdiddle Ben) ?who)

;first query system looks for an assertion matching the pattern. There are none.
;then it looks for rule conclusions matching the pattern,finds one, creates a frame with ?staff-person bound to (Bitdiddle Ben)
;and the body is evaluated in this frame
;all assertions matching (supervisor (Bitdiddle Ben) ?boss) are found (there happens to be one only)
;the and-query is evaluated in parallel to the above matching
;the system evaluates (outranked-by (Bitdiddle Ben) ?middle-manager)
;which is the same as the original query
;and with this definition of outranked-by the evaluation will keep recuring with the same constant and variable, never halting


;Exercise 4.65
(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))
;asking (wheel ?who) results in finding all bindings for ?middle-manager such that ?who is the supervisor of ?middle-manager
;Oliver Warbucks is a direct supervisor of three people, so the output the first element of the query is three frames
;now, for each frame, the system finds all binding for ?x such that ?middle-manager supervises ?x
;there are three such bindings in the frame in whcih ?middle-manager is bound to (Bitdiddle Ben)
;one such binding in the frame in which ?middle-manager is bound to (Scrooge Eben)
;and none in the frame in which ?middle-manager is bound to (Aull DeWitt)
;so, the final result is a stream with 5 frames (the one where ?who is bound to Alyssa is not discussed), 4 of which bind ?who to Oliver
;the original query is instantiated in each frame, hence (whell (Oliver Warbucks)) occurs 4 times


;Exercise 4.66
;why the simple accumulation scheme won't work?
;because it relies on an assumption that every assertion fitting the query will appear in the output stream only once.
;one way to solve the problem is to filter the output stream of assertions so that it does contain duplicates


;Exercise 4.67
;loop detector
;the system should maintain some sort of history of its deductions and should not begin processing a query it is already working on
;what needs to be stored is the query that is processed and the environment in which it is processed
;during query evaluation we'll have to evaluate other queries (unless we're dealing with the simplest case of a pattern matching assertions directly)
;whenever a new query is encountered, we check if the pattern is the same as the one we're currently evaluating
;if not, it's safe to proceed
;if yes, we check if the environment is the same as the environment of the original query. If it contains more bindings of the pattern variables, then it's safe to proceed
;if no, we're getting into a loop, and evaluation should be stopped
;if we encounter no loops, the query pattern and environment pair gets thrown of the stack


;Exercise 4.68
(rule (reverse (?x) (?x)))
(rule (reverse (?h . ?t) ?y) 
      (and (reverse ?t ?reversed-t)
           (append-to-form ?reversed-t (?h) ?y)))

;evaluating (reverse ?x (1 2 3))
;?x is unbound, so it gets bound to (?h . ?t)
;evaluating (reverse ?t ?reversed-t)
;?t is unbound, so it gets bound to (?h2 . ?t2)
; ...


;Exercise 4.69
(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mahujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)

(rule (grandson ?G ?S)
      (son ?G ?F)
      (son ?F ?S))

(rule (son ?D ?S)
      (or (son ?D ?S)
          (and (son ?W ?S)
               (wife ?D ?W))))

(rule (ends-in-grandson (grandson)))
(rule (ends-in-grandson (?x . ?y))
      (ends-in-grandson ?y))

(rule ((great . ?rel) ?A ?B)
      (and (ends-in-grandsand ?rel)
           (and (son ?A ?C)
                (?rel ?C ?B))))