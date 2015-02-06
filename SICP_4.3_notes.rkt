#lang racket

;Nondeterministic computing
;finding a and b whose sum is prime
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

;expressions in a nondeterministic language can have more than one possible value
;nondeterministic program evaluator needs to choose a possible value and keep track of the choice

(list (amb 1 2 3) (amb 'a 'b))
;has 6 possible values
;amb with a single choice produces ordinary, determined answer
;amb with no choices is an expression with no acceptable values- when it's evaluated, computation is aborted and returns no value

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

;that's kind of mind bending
;as in, suddenly I feel like there's no control over the computation

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))
;this returns as integer equal of greater than n

;  but how do we keep track of what has been already returned?
;  or is it ok to return the same thing more than once?
;  nah, should not be. We may care about inspecting all elements, as in prime-sum-pair
;  if so, it would be better to inspect each element only the minimum necessary amount of times, rather than stumble haplessly hoping to find what we're searching for

;evaluating amb represents a nondeterministic choice point
;it's as if computation was branching
;it could actually branch, and all possibilities could be explored, given sufficient number of processors
;but, if only one process can be executed at a time, possibilities must be explored sequentially
;the evaluator might just pick a branch at random, but systematicity is better
;first, pick the first alternative at each choice point
;if evaluation fails, backtrack to the most recent amb and pick the next alternative
;if there are no more possibilities at the most recent amb, backtrack one step futher - depth-first search
;that is certainly methodical, but seems like it can produce a lot of redundancy

;Driver loop
;returns the value of the first non-failing execution
;if we want the next one, the interpreter has to backtrack an look for a second value
;backtracking is only available for one expression at a time
;i.e. if the interpreter evaluates one exp and then gets a second exp to deal with, it will no longer be possible to look for another possible value of the first exp


;Nondeterministic programs
;why would you even want to have a nondeterministic interpreter?
;to supress the details of how search is carried out
;-solving logic puzzles

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (copper (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (distinct? (list baker copper miller fletcher smith)))
    (require (not (= baker 5)))
    (require (not (= copper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller copper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher copper)) 1)))
    (list (list 'baker baker) (list 'copper copper)
          (list 'miller miller) (list 'fletcher fletcher)
          (list 'smith smith))))



;Parsing natural language
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

;there is a global variable, *unparsed*, which holds the unparsed part of the sentence

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found word)))

;why is nondeterministic evaluation a good fit for language parsing?
;1: require is a convenient way of expressing constraints
;2: in non-toy grammars there are very many possible choices for how words can be composed into phrases
;often you need to explore different possibilities to get a valid parse

(define prepositions '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (pars-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase verb-phrase (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))