#lang racket

;Exercise 4.17

;With internal defines:

;we call procedure p in the global-environment
;this creates evaluation-environment for p
;when we encounter the first internal definition, we add a binding to the evaluation environment
;body of the definition is not evaluated, only bound to the name of the function
;the same happens with the second internal definition
;when the actuall action expression is evaluated, the evaluation environment will contain bindings for u and v
;and all other bindings will be found in the global environment


;With internal let and assignment:
;let is implemented as a lambda with let-variable being its formals and let-body being its body, applied to the let-values
;we call p in the global environment and create an environment for evaluating p
;p calls an anonymous function, creating another nested environment
;withing this environment the formals of the anonymous function (u v) are bound to the arguments to which the function is applied (*unassigned* *unassigned*)
;then, the body of the function is evaluated
;i.e. u is set to exp1, v is set to exp2, and exp3 is evaluated