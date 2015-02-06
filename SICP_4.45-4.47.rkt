#lang racket

;Exercise 4.45
"The professor lectures to the student in the class with the cat"
[the professor [lectures [to [the student]] [in [the class]] [with [the cat]]]] ;professor lectures with a cat, lectures to the student, and lectures in the class
[the professor [lectures [to [the student]] [in [the class [with [the cat]]]]]] ;there's a cat in the class in which professor lectures
[the professor [lectures [to [the student [in [the class]]]] [with [the cat]]]] ;he lectures to a student who is in the class, and he lectures with a cat
[the professor [lectures [to [the student [in [the class [with [the cat]]]]]]]] ;he lectures to a student who is in the class in which the cat is
[the professor [lectures [to [the student [in [the class]] [with [the cat]]]]]] ;he lectures to a student who is in the class and who has/is with the cat
;The last one is really unintuitive


;Exercise 4.46
;amb evaluator evaluates operands from left to right. Why is it important?
;e.g.
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))
;we need to evaluate (parse-word prepositions) before (parse-noun-phrase)
;both operations access the *unparsed* variable, and in the *unparsed* P comes before NP if we're dealing with a PP
;if we started by evaluating (parse-noun-phrase), we wouldn't get anywhere, because (car *unparsed*) would be a P, with which parse-noun-phrase cannot deal

;Exercise 4.47
(define (new-parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (new-parse-verb-phrase)
             (parse-prepositional-phrase))))

(define (original-parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase verb-phrase (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

;in the original version, first we try parsing a verb using (parse-word verbs)
;if we don't succeed, we try to parse a V followed by a PP
;which means that we first try [V [PP]], and if it doesn't work, we try to parse [[V [PP]] [PP]]

;in the new version we do away with the helper function
;we start by trying to parse a V using (parse-word verbs)
;if it doesn't work, we make a list which says that the VP is a V followed by a PP
;but nothing happens with this list...

;Anyway, is the order of amb arguments was changed we would never try simply parsing a verb without a PP
;instead, with each failure we would go deeper into PP recursion
