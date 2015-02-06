#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Project 4
;;;  exercises

;;;  Warmup exercise 1
; What kind of value does (ask me 'location) return?
; me is an avatar handler
; 'location message is handled by the mobile-thing part of an avatar
; asking a mobile thing for location returnes a place handler, i.e. a procedure
; Place handler can be asked to list the exits from the place, say whether there's an exit in a given direction, and add an exit.
; Apart from that, a place can be asked about its name, people and things it contains, it can be instructed to add or loose a thing -anything a named object or a container can do.

;;; Warmup exercise 2 & 3
; drawn

;;; Warmup exercise 4
; There are usual things scattered around, trees, books, blackboard etc.
; and there are spells, randomly dispersed throughout the rooms
; As for people, there are 4 students, 2 hall monitors, and 2 trolls

;;; Warmup exercise 5
; evalution environment diagrams...

;;; Warmup exercise 6
; playing around


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Exercise 1
; show doesn't work
; because I cannot think of any way to work around Racket's lack of first class environments
; and beacuse racket seems to lack a function for printing out the source code of a given procedure

;;; Exercise 2
                  
'HAS-A
(lambda (type)
  (let* ((things (ask self 'THINGS))
         (wanted-things (filter (lambda (x) (member? type (ask x 'TYPE))) things)))
    (if (null? wanted-things)
        (ask self 'SAY (list "Nah, I haven't got any of that."))
        (ask self 'SAY (list "Hmm, I have" wanted-things)))))

'HAS-A-THING-NAMED
(lambda (name)
  (let* ((things (ask self 'THINGS))
         (the-thing (filter (lambda (x) (eq? (ask x 'NAME) name)) things)))
    (if (null? the-thing)
        (ask self 'SAY (list "I haven't got the " name))
        (ask self 'SAY (list "I've got the " name)))))


;;; Exercise 3
      'FEEL-THE-FORCE
(lambda ()
  (for-each (lambda (x) (ask screen 'TELL-WORLD (list (ask x 'NAME) "is at" (ask (ask x 'LOCATION) 'NAME))))
            (all-people)))


;;; Exercise 4
(define (ring-of-obfuscation self name location)
  (let ((mobile-thing-part (mobile-thing self name location)))
    (make-handler
     'ring-of-obfuscation
     (list 'method)
     mobile-thing-part)))

(define (create-ring name location)
  (create-instance ring-of-obfuscation name location))

(define (populate-rings rooms)
  (for-each (lambda (room)
              (if (= (random 8) 0)
                  (ask room 'add-thing (create-ring 'ring room))))
            rooms))

'PEOPLE-AROUND
(lambda ()
  (filter (lambda (x) (map (lambda (y) (member? 'ring-of-obfuscation (ask y 'TYPE)))
                           (ask x 'THINGS)))
          (delq self (find-all (ask self 'LOCATION) 'PERSON))))

'FEEL-THE-FORCE
(lambda ()
  (for-each (lambda (x) (ask screen 'TELL-WORLD (list (ask x 'NAME) "is at" (ask (ask x 'LOCATION) 'NAME))))
            (filter (lambda (x) (map (lambda (y) (member? 'ring-of-obfuscation (ask y 'TYPE)))
                                     (ask x 'THINGS)))
                    (all-people))))


;;; Exercise 5
(define (wand self name birthplace)
  (let ((mobile-thing-part (mobile-thing self name birthplace)))
    (make-handler
     'wand
     (make-methods
      'ZAP
      (lambda (target)
        (let ((owner (ask self 'LOCATION)))
        (if (not (member? 'person (ask owner 'TYPE)))            
            (ask self 'EMIT (list "Wands can only be used by people!")))
        (let ((spells (filter (lambda (x) (member? 'spell (ask x 'TYPE)))
                              (ask owner 'THINGS))))
          (if (null? spells)
              (ask self 'EMIT (list "There are no spells to be used."))
              (let ((spell (pick-random spells)))
                (begin (ask owner 'EMIT (list "I am zapping a wand at" (ask target 'NAME)
                                                 ", saying " (ask spell 'INCANT)))
                       (apply (ask spell 'ACTION) (list owner target))))))))
      'WAVE
      (lambda ()
        (let ((owner (ask self 'LOCATION)))
        (if (not (member? 'person (ask owner 'TYPE)))            
            (ask self 'EMIT (list "Wands can only be used by people!")))
        (let ((things (ask owner 'PEOPLE-AROUND)))
          (if (null? things)
              (ask self 'EMIT (list "There's nothing here to cast a spell on."))
              (ask self 'ZAP (pick-random things)))))))
     mobile-thing-part)))

(define (create-wand name location)
  (create-instance wand name location))

(define (populate-wands rooms)
  (map (lambda (wand-type) (create-wand wand-type (pick-random rooms)))
       '(unicorn-hair-wand vampire-tooth-wand
         hydra-eye-wand mermaid-scale-wand
         centaur-heart-wand dragon-blood-wand)))

;;; Exercise 6
(define (instantiate-spells)
  (let ((chamber (create-place 'chamber-of-stata)))
    (create-spell
     'boil-spell
     chamber
     "habooic katarnum"
     (lambda (caster target)
       (if (member? 'person (ask target 'TYPE))
       (ask target 'EMIT
            (list (ask target 'NAME) "grows boils on their nose"))
       (ask screen 'TELL-WORLD (list "You cannot grow boild on things!")))))
    (create-spell
     'slug-spell
     chamber
     "dagnabbit ekaterin"
     (lambda (caster target)
       (if (member? 'person (ask target 'TYPE))
       (begin (ask target 'EMIT (list "A slug comes out of" (ask target 'NAME) "'s mouth."))
       (create-mobile-thing 'slug (ask target 'LOCATION)))
       (ask screen 'TELL-WORLD (list "Slug spell works on people only.")))))
    chamber))

(create-spell
 'wind-of-doom
 chamber
 "floo domdaradum"
 (lambda (caster target)
   (ask caster 'EMIT (list "Let the wind of doom blow!"))
   (if (member? 'person (ask target 'TYPE))
       (ask target 'SUFFER (random-number 2) self))
       (ask target 'DESTROY)))
       

;;; Exercise 7
(define (wit-student name birthplace activity miserly)
  (let ((autonomous-person-part (autonomous-person name birthplace activity miserly)))
    (make-handler
     'wit-student
     (make-methods
      'INSTALL
      (lambda ()
        (ask autonomous-person-part 'INSTALL)
        (ask clock 'ADD-CALLBACK
             (create-clock-callback 'zap-someone self 
                                    'ZAP-SOMEONE)))
      'ZAP-SOMEONE
      (lambda ()
        (let ((target (pick-random (ask self 'PEOPlE-AROUND)))
              (wand (pick-random (filter (lambda (x) (ask x 'IS-A 'wand))
                                         (ask self 'THINGS)))))
          (cond ((and target wand) (ask wand 'ZAP 'target))
                ((and (not target) wand) (ask wand 'WAVE)))))
      'DIE
      (lambda (perp)
        (ask clock 'REMOVE-CALLBACK self 'zap-someone)
        (ask autonomous-person-part 'DIE perp)))
    autonomous-person-part)))         
              

; in populate players:
(students-wands (map (lambda (person) (create-wand
                                       (pick-random '(unicorn-hair-wand vampire-tooth-wand
                                                                        hydra-eye-wand mermaid-scale-wand
                                                                        centaur-heart-wand dragon-blood-wand))
                                       person))
                     students))


;;; Exercise 8
(define (create-wit-professor name birthplace activity miserly)
  (create-instance wit-professor name birthplace activity miserly))

(define (wit-professor self name birthplace activity miserly)
  (let ((student-part (wit-student self name birthplace activity miserly)))
    (make-handler
     'wit-professor
     (make-methods
      'INSTALL
      (lambda ()
        (ask student-part 'INSTALL)
        (ask clock 'ADD-CALLBACK
             (create-clock-callback 'teach-a-spell self 
                                    'TEACH-A-SPELL)))
      'TEACH-A-SPELL
      (lambda ()
        (let ((pupil (pick-random (filter (lambda (x) (ask x 'IS-A 'wit-student))
                                          (ask self 'PEOPLE-AROUND)))))
          (if pupil
              (let ((spell (pick-random (ask chamber-of-stata 'THINGS))))
                (begin (clone-spell spell pupil)
                       (ask screen 'TELL-WORLD (list (ask self 'NAME) "teaches" (ask spell 'NAME)
                                                     "to" (ask pupil 'NAME))))))))
      'DIE
      (lambda (perp)
        (ask clock 'REMOVE-CALLBACK self 'teach-a-spell)
        (ask student-part 'DIE perp)))
     student-part)))


;;; Exercise 9

;changes in spell:
'USE
(lambda (caster target)
  (let ((protection (pick-random (filter (lambda (x) (eq? (ask x 'NAME) counterspell))
                                         (ask target 'THINGS)))))
    (if protection
        (ask protection 'COUNTERACT self)
        (action caster target))))


(define (counterspell self name location incant spell)
  (let ((mobile-part (mobile-thing self name location)))
    (make-handler
     'counterspell
     (make-methods
      'INCANT
      (lambda () incant)
      'PROTECT
      (lambda (perp)
        (ask (ask self 'LOCATION) 'SAY (list "I counter your" spell))))
     mobile-part)))

(define (instantiate-spells-and-counterspells)
  (let ((chamber (create-place 'chamber-of-stata)))
    (create-spell
     'boil-spell
     chamber
     "habooic katarnum"
     (lambda (caster target)
       (if (ask target 'IS-A 'person)
           (ask target 'EMIT (list (ask target 'NAME) "grows boils on their nose."))
           (ask screen 'TELL-WORLD
                (list "Boil spell will not work on" (car (ask target 'TYPE))))))
     'boil-yourself-spell)
    (create-counterspell
     'boil-yourself-spell
     chamber
     "cioobah munratak"
     'boil-spell)
    
    (create-spell
     'slug-spell
     chamber
     "dagnabbit ekaterin"
     (lambda (caster target)
       (if (ask target 'IS-A 'person)
           (begin (ask target 'EMIT (list "A slug comes out of" (ask target 'NAME) "'s mouth."))
                  (create-mobile-thing 'slug (ask target 'LOCATION)))
           (ask screen 'TELL-WORLD
                (list "Slug spell will not work on" (car (ask target 'TYPE))))))
     'slugless-spell)
    (create-counterspell
     'slugless-spell
     chamber
     "tibbangad niretake"
     'slug-spell)
    
    (create-spell
     'wind-of-doom
     chamber
     "floo domdaradum"
     (lambda (caster target)
       (ask caster 'EMIT (list "Let the wind of doom blow!"))
       (if (ask target 'IS-A 'person)
           (ask target 'SUFFER (random-number 2) caster)
           (begin (ask target 'DESTROY)
                  (ask screen 'TELL-WORLD (list (ask target 'NAME) "has been destroyed.")))))
     'still-air-spell)
    (create-counterspell
     'still-air-spell
     chamber
     "oolf mudaradmod"
     'wind-of-doom)
    
    chamber))
    
#|
(define (instantiate-spells)
  (let ((chamber (create-place 'chamber-of-stata)))
    (create-spell
     'boil-spell
     chamber
     "habooic katarnum"
     (lambda (caster target)
       (if (ask target 'IS-A 'person)
           (ask target 'EMIT (list (ask target 'NAME) "grows boils on their nose."))
           (ask screen 'TELL-WORLD
                (list "Boil spell will not work on" (car (ask target 'TYPE)))))))
    (create-spell
     'slug-spell
     chamber
     "dagnabbit ekaterin"
     (lambda (caster target)
       (if (ask target 'IS-A 'person)
           (begin (ask target 'EMIT (list "A slug comes out of" (ask target 'NAME) "'s mouth."))
                  (create-mobile-thing 'slug (ask target 'LOCATION)))
           (ask screen 'TELL-WORLD
                (list "Slug spell will not work on" (car (ask target 'TYPE)))))))
    (create-spell
     'wind-of-doom
     chamber
     "floo domdaradum"
     (lambda (caster target)
       (ask caster 'EMIT (list "Let the wind of doom blow!"))
       (if (ask target 'IS-A 'person)
           (ask target 'SUFFER (random-number 2) caster)
           (begin (ask target 'DESTROY)
                  (ask screen 'TELL-WORLD (list (ask target 'NAME) "has been destroyed."))))))
    chamber))
|#


;;; Exercise 10
(define (create-chosen-one name birthplace activity miserly)
  (create-instance chosen-one name birthplace activity miserly))

(define (chosen-one self name birthplace activity miserly)
  (let ((student-part (wit-student self name birthplace activity miserly)))
    (make-handler
     'chosen-one
     (make-methods
      'SUFFER
      (lambda (hits perp)
        (if (<= (ask self 'HEALTH) hits)
            (begin (ask self 'EMIT (list "The scar flares so bright!"))
                   (ask perp 'DIE self))
            (ask student-part 'SUFFER hits perp))))
     student-part)))

      
 


