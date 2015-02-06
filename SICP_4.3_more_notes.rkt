#lang racket

;AMB EVALUATOR

;based on the analyzing evaluator
;evaluation of an expression is accomplished by calling an execution procedure produced by analysis of that expression 

;execution procedures will take 3 arguments: environment and two continuation procedures
;if evaluation results in a value, the successs continuation will be called
;if evaluation leads to a dead end, the failure continuation will be called
;success continuation takes the value and proceeds with computation. It also tahes another argument, ehich is a failure continuation, in case dead end is reached
;failure continuation explores another branch of the nondeterministic process
;if some side effects occured on the present line of evaluation, and we have to backtrack to a point before these side effects happened
;it is necessary to undo these side effects before exploring a new path
;so side-effect operations must produce failure continuations as well

;failure continuations are produced by:
; - amb expressions
; - top-level driver (a mechanism to report failure when choices are exhausted)
; - assignments

;failure initiators:
; - (amb)
; - try-again typed at the top-level driver

;failure continuations call other failure continuations:
; - failure continuation which undoes an assignment calls failure continuation it intercepted, so that the failure is propagated back to the choice point
; - if a failure continuation for one amb runs out of choices, it calls failre continuation of an earlier amb


