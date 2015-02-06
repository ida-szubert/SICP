#lang racket

;Exercise 4.55
(supervisor (Ben Bitdiddle) ?x)
(job ?x (accounting . ?y))
(address ?x (Slumerville . ?y))

;Exercise 4.56
(and (supervisor (Ben Bitdiddle ?person))
     (address ?person ?address))
(and (salary (Ben Bitdiddle) ?ben-salary)
     (salary ?person ?amount)
     (lisp-value > ?ben-salary ?amount))
(and (not (job ?supervisor (computer . ?type)))
     (supervisor ?person ?supervisor)
     (job ?supervisor ?job))

;Exercise 4.57
(rule (can-replace ?person1 ?person2)
      (and (or (and (job ?person1 ?job)
                    (job ?person2 ?job))
               (can-do-job (job ?person1 ?job1)
                           (job ?person2 ?job2)))
           (not (same ?person1 ?person2))))

(can-replace ?who (Fect Cy D))

(and (salary ?person1 ?amount1)
     (salary ?person2 ?amount2)
     (lisp-value > ?amount2 ?amount1)
     (can-replace ?person1 ?person2))

;Exercise 4.58
(rule (big-shot ?person ?division)
      (and (job ?person (?division . ?x))
           (supervisor ?person ?sup)
           (not (job sup? (?division . ?y)))))

;Exercise 4.59
;a.
(meeting ?x (Friday ?y))

;b.
(rule (meeting-time ?person ?day-and-time)
      (or (and (job ?person (?division . ?x))
               (meeting ?division ?day-and-time))
          (meeting whole-company ?day-and-time)))

;c.
(meeting-time (Hacker Alyssa P) (Wednesday ?time))

;Exercise 4.60
??


