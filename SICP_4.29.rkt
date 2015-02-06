#lang planet neil/sicp

;Exercise 4.19

;with memoization
; > (square (id 10))
; 100
; > count
; 1
; > (square (id 10))
; 100
; > count
; 2
; > (square (id 10))
; 100
; > count
; 3

;without memoization:
; > (square (id 10))
; 100
; > count
; 2
; > (square (id 10))
; 100
; > count
; 4
; > (square (id 10))
; 100
; > count
; 6

;it seems that without memoization (id 10) is evaluated two times with each call to square
;meaning that the x of square gets evaluated every time it appears in the body

;with memoization, the x of square gets evaluated only the first time it appears
;the second time we just retrieve the stored value
