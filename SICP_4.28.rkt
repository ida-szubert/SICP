#lang planet neil/sicp

;Exercise 4.28
;in the application? clause, eval uses actual-value to get the value of the operator that is to be applied
;why doesn't it use eval?

;actual-value is
(force-it (eval exp env))
;so the difference is in the force-it procedure wrapped around eval

;(force-it (eval exp env)) is the same as (eval exp env) if (eval exp env) is not a thunk
;but how can an evaluation procedure become a thunk?
;(eval exp env) would have to be one ofthe arguments of a compound procedure



;I AM VERY CONFUSED