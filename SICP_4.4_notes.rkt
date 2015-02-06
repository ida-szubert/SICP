#lang racket

;LOGIC PROGRAMMING
;expression-oriented languages are based on the fact that an expression describing a value islso a means of computing that value
;direction of computation is well-specified
;from the input, which is fed into a function, to the output

;but there are also constraint-based systems where directionality is not so rigid
;the system itself needs to provide declarative knowledge which programmer provides when writing in expression-oriented way

;Unification
;'what-is' fact can be used to solve a number of problems with different 'how-to' components
;e.g.
;append procedure embodies two rules
;for any y, '() and y append to form y
;for any u, v, y, z (cons u v) and y append to (cons u z) if v and y append to form z
;these rules are sufficient not only to find l which is an append of x and y
;but also to find x given y and l
;and find possible x and y given l

;in logic programming, we state such rules, and let the interpreter worry about what steps to take to solve a given problem
;BUT: there my be many mathematically equivalent sets of rules, only some of which can be effective devices for computing in any direction
;also, some 'what-is' give no clue about 'how-to'
;   although I cannot see the difference right now

;logic language is apparently a good choice for a language to make queries to databases with

(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))


(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))


(supervisor (Bitdiddle Ben) (Warbucks Oliver))
(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)


(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))


(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))

(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))
(can-do-job (computer programmer)(computer programmer trainee))
(can-do-job (administration secretary) (administration big wheel))

;Query
(job ?x (computer programmer))
;we're looking for entries that match a certain pattern
;i.e. the first element of the list is 'job, the second in whatever, and the third is '(computer programmer)
;pattern variables have names, so that we can specify that we're expecting the same whatever to appear in all the places where we use the same variable
(supervisor ?x ?x) ;looks for records of people who supervise themselves
;a variable stands for one thing
(job ?x (computer y?)) ;does not find (computer programmer trainee)
(job ?x (computer . y?)) ;does

;the system finds all sets of values for the variables in the query such that if the variables are replaced by the values, the result is in the database

;compund queries can be formed using and, or, not, and lisp-value
;lisp value is used to select those entries that fulfll some requirements that are not captured by pattern-variable matching
;it uses a predicate and a suitable number of arguments, and an entry will fit a query if the predicate returns #t
(and (salary ?person ?amount) (lisp-value > ?amount 30000))


;Abstracting queries through rules
(rule (lives-near ?person1 ?person2)
      (and (address ?person1 (?town . ?rest1))
           (address ?person2 (?town . ?rest2))
           (not (same ?person1 ?person2))))

(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))
;rules are kind of shortcuts to compound queries
;they consist of a conclusion (e.g. whoever fits ?person is a wheel) and a body
;when using a rule we don't neccesarily get a record from the database, but an appropriate assertion, which can be an inference from the facts in the database
;(wheel (Warbucks Oliver))
;rules are like procedures in that they can be used as parts of other rules

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))


(append-to-form x y z)
(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

(append-to-form (a b) (c d) ?z) ;finds the result of appending
(append-to-form (a b) ?x (a b c d)) ;finds one of the appended lists
(append-to-form ?x ?y (a b c d)) ;finds two lists that form the third when appended


;Pattern matching and unification

;pattern matching works by taking a frame, the pattern, and the data base
;and producing extensions to the frame such that they contain appropriate bingings for the pattern variables
;appropriate bindings are those that generate entries from the database when values are substituted for variables in the pattern

;pattern matching can work by using streams
;stream of input frames
;stream of assertions from the database
;and output stream of extended frames
;it is obvious that for a given pattern and database there might be more than one frame extension generating a valid assertion
;so, it is obvious that the output of a pattern matcher is a stream (or generaly, a list) of frames
;the input needs to be a stream (list) as well. whac can be easily seen by thinking about compound queries
;e.g. and 'and' query works by producing all the frame extensions that match the first pattern, and feeding them into a second pattern matcher
;the second matcher tries to extend these frames (in which some patter variables are already bound) so that they match the second pattern
;therefore, pattern matchers need to be able to accept many frames as input

;unification is, in a way, a generalization of pattern matching
;a unifier takes two patterns, each with constants and variables, and tries to find such a frame extension in which the patterns would be equal
;patterns cannot be unified if they have conflicting requirements onn the variable bindings, or conflicting constants
;unifing patterns can be thought of as solving a set of equations

(unify (?x ?x) ((a ?y c) (a b ?z))) ;is like solving the following simultaneous equations
?x = (a ?y c)
?x = (a b ?z)
(a ?y c) = (a b ?z)

;successful unification does not need to result in a frame in which all variables are bound
;there may be not enough info to do that
;but as long as thre's nothing inconsistens about the unbound variables, unification succeeds
;'nothing inconsitent' menas that there are possible non-conflicting bindings of the remaining variables
;unification produces the most general pattern which expresses the constraints expressed by the two original patterns


;Rules
;making inferences from rules depends on unification
;when given a rule-involving query, the system will firs treat it as a simple pattern matching case
;then it will try to unify the query with the conclusions of the defined rules
;relative to the frame produced by successful unification, the system evaluates the compund query given by the body of the rule