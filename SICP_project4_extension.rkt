#lang racket

;;; Exercise 11

;time-turner
;would transport the user back in time by n clock ticks
;from that point on, everything would go as it did before, except from the fact that there'd be an additional person
;in the second timeline, the original user avatar would act like an autonomous person
;and a new avatar would be created
;after n clock tick, it's back to one timeline, and the second avatar disappears
;everything needs to be merged- whatever the differences between timeline 1 and timeline2, timeline2 states take precedence
;in the second timeline, avatar2 cannot interact with avatar1
;otherwise, sth bad happens
;using time-turner when in timeline 2 is prohibited
;it should go inactive

;time turner would be a mobile thing
;in timeline2, however, it should be inactive, i.e. it cannot be dropped, stolen, etc

;how to record what happens during eeach turn?
;let's say that using time-turner is limited to 4 clock ticks
;at each point in the game, last 4 turns need to be stored

;what is a turn?
;everything that happend when a clock ticks
;all movements and actions of all characters
;let's have a global variable 'current-turn'
;when a procedure is evaluated, it is also stored away in 'current-turn'
;but not every procedure
;(ask x 'DIE) should be stored, but stuff like (apply method args) doesn't need to
;so, procedures which determine events need to be stored, but not procedure that actuate these events
;also, whenever there's randomness involved, the random value needs to be stored somehow and reused in timeline2
;this applies particularily to 'go-somewhere, 'do-something, 'teach-a-spell, 'zap, 'wave
;i'd need procedures alternative to these, one that take an argument rather than chose it randomly

;maybe I need a whole set of timeline2 objects, exhibiting no randomness at all?







;IDEA 2
;buffer
;collect all the messages
;first say where people go
;then, what they do, room-wise

;or maybe each turn clock callbacks should be rearranged
;so that first everyone moves, and then they do stuff
;autonomous person would have a separate callback for moving and for doing stuff
;but now all callbacks are rearranged at each clock tick
;seems inefficient
;maybe it'd be better to add callbacks in such a way that they are allways in the correct order?