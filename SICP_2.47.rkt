#lang planet neil/sicp

;Exercise 4.27
;input: count
;value: 1
;input: w
;value: 10
;input: count
;value: 2

;count is defined as a variable, whose value is 0
;on each call, id sets count to +1, and returns its argument
;id is identity function which records in count how many times it's been called 
;w calls (id 10) twice

;the results are surprising
;count outputs 1 instead of 0, ever though nothing seems to have happened in between defining count and calling it

;when w is defined, id is evaluated once only, since (id 10) is delayed
;hence the response to count is 1

;then w is called
;the response is 10, as expected
;and when count is called again, the response is 2, when one might have expected 3
;that's because only the delayed argument, (id 10), is evaluated this time round

;And that's really confusing


