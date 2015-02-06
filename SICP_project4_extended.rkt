#lang planet neil/sicp

(#%require (only racket/base format))
(#%require (only racket/base procedure-arity))


;;------------------------------------------------------------
;; Instance


(define (make-instance)
  (list 'instance #f))

(define (instance? x)
  (and (pair? x) (eq? (car x) 'instance)))

(define (instance-handler instance) (cadr instance))

(define (set-instance-handler! instance handler)
  (set-car! (cdr instance) handler))

(define (create-instance maker . args)
  (let ((instance (make-instance)))
    (let ((handler (apply maker instance args)))
      (set-instance-handler! instance handler)
      (if (method? (get-method 'INSTALL instance))
          (ask instance 'INSTALL))
      instance)))


;;------------------------------------------------------------
;; Handler

(define (make-handler typename methods . super-parts)
  (cond ((not (symbol? typename))
         (error "bad typename" typename))
        ((not (method-list? methods))
         (error "bad method list" methods))
        (else
         (lambda (message)
           (case message
             ((TYPE)
              (lambda () (type-extend typename super-parts)))
             ((METHODS)
              (lambda ()
                (append (method-names methods)
                        (append-map (lambda (x) (ask x 'METHODS))
                                    super-parts))))
             (else
              (let ((entry (method-lookup message methods)))
                (if entry
                    (cadr entry)
                    (find-method-from-handler-list message super-parts)))))))))


(define (->handler x)
  (cond ((instance? x) (instance-handler x))
        ((and (procedure? x) (= (procedure-arity x) 1)) x)
        (else (error "I don't know how to make a handler from" x))))

(define (make-methods . args)
  (define (helper lst result)
    (cond ((null? lst) result)
          ((null? (cdr lst)) (error "unmatched method (name,proc) pair"))
          ((not (symbol? (car lst))) (error "invalid method name" (car lst)))
          ((not (procedure? (cadr lst))) (error "invalid method procedure" (cadr lst)))
          (else (helper (cddr lst) (cons (list (car lst) (cadr lst)) result)))))
  (cons 'methods (reverse (helper args '()))))

(define (method-list? methods)
  (and (pair? methods) (eq? (car methods) 'methods)))

(define (empty-method-list? methods)
  (null? (cdr methods)))

(define (method-lookup message methods)
  (assq message (cdr methods)))

(define (method-names methods)
  (map car (cdr methods)))

;;------------------------------------------------------------
;; Root Object


(define (root-object self)
  (make-handler
   'root
   (make-methods
    'IS-A
    (lambda (type)
      (memq type (ask self 'TYPE))))))

;;------------------------------------------------------------
;; Object Interface

(define (ask object message . args)
  (let ((method (get-method message object)))
    (cond ((method? method)
           (apply method args))
          (else
           (error "No method for" message 'in 
                  (safe-ask 'UNNAMED-OBJECT
                            object 'NAME))))))

(define (safe-ask default-value obj msg . args)
  (let ((method (get-method msg obj)))
    (if (method? method)
        (apply ask obj msg args)
        default-value)))

;;--------------------
;; Method Interface
;;

(define (get-method message . objects)
  (find-method-from-handler-list message (map ->handler objects)))

(define (find-method-from-handler-list message objects)
  (if (null? objects)
      (no-method)
      (let ((method ((car objects) message)))
        (if (not (eq? method (no-method)))
            method
            (find-method-from-handler-list message (cdr objects))))))

(define (method? x)
  (cond ((procedure? x) #T)
        ((eq? x (no-method)) #F)
        (else (error "Object returned this non-message:" x))))

(define no-method
  (let ((tag (list 'NO-METHOD)))
    (lambda () tag)))

(define (type-extend type parents)
  (cons type 
        (remove-duplicates
         (append-map (lambda (parent) (ask parent 'TYPE))
                     parents))))

(define (append-map f list)
  (if (null? list)
      '()
      (append (f (car list)) (append-map f (cdr list)))))
;;------------------------------------------------------------
;; Utility procedures

(define (random-number n)
  (+ 1 (random n)))

(define (pick-random lst)
  (if (null? lst)
      #F
      (list-ref lst (random (length lst)))))

(define (find-all source type)
  (filter (lambda (x) (ask x 'IS-A type))
          (ask source 'THINGS)))

(define (delq item lst)
  (cond ((null? lst) '())
        ((eq? item (car lst)) (delq item (cdr lst)))
        (else (cons (car lst) (delq item (cdr lst))))))

(define (filter predicate lst)
  (cond ((null? lst) '())
        ((predicate (car lst))
         (cons (car lst) (filter predicate (cdr lst))))
        (else (filter predicate (cdr lst)))))

(define (fold-right op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (fold-right op init (cdr lst)))))

(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates (filter (lambda (x) 
                                         (not (eq? x (car lst))))
                                       lst)))))



;;------------------------------------------------------------
;; Support for Objects in a Simulation World

;;--------------------
;; Clock
;;

(define (a-clock self . args)
  (let ((root-part (root-object self))
        (name (if (not (null? args))
                  (car args)
                  'THE-CLOCK))
        (the-time 0)
        (callbacks '())
        (removed-callbacks '()))
    (make-handler
     'clock
     (make-methods
      'INSTALL
      (lambda ()
        (ask self 'ADD-CALLBACK
             (create-clock-callback 'tick-printer self 'PRINT-TICK)))
      'NAME      (lambda () name)
      'THE-TIME  (lambda () the-time)
      'RESET     (lambda ()
                   (set! the-time 0)
                   (set! callbacks '()))
      'TICK
      (lambda ()
        (set! removed-callbacks '())
        (for-each (lambda (x) 
                    (if (not (memq x removed-callbacks))
                        (ask x 'activate)))
                  (arrange-callbacks callbacks))
        (set! the-time (+ the-time 1)))
      'ADD-CALLBACK
      (lambda (cb)
        (cond ((not (ask cb 'IS-A 'CLOCK-CALLBACK))
               (error "Non callback provided to ADD-CALLBACK"))
              ((null? (filter (lambda (x) (ask x 'SAME-AS? cb))
                              callbacks))
               (set! callbacks (add-to-callbacks cb callbacks))
               'added)
              (else
               'already-present)))
      'REMOVE-CALLBACK
      (lambda (obj cb-name)
        (set! callbacks 
              (filter (lambda (x) 
                        (cond ((and (eq? (ask x 'NAME) cb-name)
                                    (eq? (ask x 'OBJECT) obj))
                               (set! removed-callbacks
                                     (cons x removed-callbacks))
                               #f)
                              (else #t)))
                      callbacks))
        'removed)
      'PRINT-TICK
      (lambda ()
        (ask screen 'TELL-WORLD
             (list #\newline "---" (ask self 'NAME) "Tick" (ask self 'THE-TIME) "---" #\newline))))
     root-part)))

(define (create-clock . args)
  (apply create-instance a-clock args))

(define (add-to-callbacks x callbacks)
  (define (help cbs)
    (cond ((null? cbs) (cons x cbs))
          ((eq? (ask (car cbs) 'name) 'move-somewhere)
           (cons (car cbs) (help (cdr cbs))))
          (else (cons x cbs))))
  (if (eq? (ask x 'name) 'move-somewhere)
      (cons (car callbacks) (cons x (cdr callbacks)))
      (help callbacks)))

;;
;; Clock callbacks
;;

(define (clock-callback self name object msg . data)
  (let ((root-part (root-object self)))
    (make-handler
     'clock-callback
     (make-methods
      'INSTALL  (lambda () 'INSTALLED)
      'NAME     (lambda () name)
      'OBJECT   (lambda () object)
      'MESSAGE  (lambda () msg)
      'ACTIVATE (lambda () (apply ask object msg data))
      'SAME-AS? (lambda (cb)
                  (and (ask cb 'IS-A 'CLOCK-CALLBACK)
                       (eq? (ask self 'NAME)
                            (ask cb 'NAME))
                       (eq? object (ask cb 'OBJECT)))))
     root-part)))

(define (create-clock-callback name object msg . data)
  (apply create-instance clock-callback name object msg data))

(define (arrange-callbacks list)
  (define (helper callbacks tick walking doing)
    (if (null? callbacks)
        (append tick walking doing)
        (let ((first (car callbacks)))
          (cond ((eq? (ask first 'name) 'tick-printer)
                 (helper (cdr callbacks) (cons first tick) walking doing))
                ((eq? (ask first 'name) 'move-somewhere)
                 (helper (cdr callbacks) tick (cons first walking) doing))
                (else (helper (cdr callbacks) tick walking (cons first doing)))))))
  (define (roomwise list ) '())
  (helper list '() '() '()))

(define (alphabetic-order? a b)
  (string<? (symbol->string (ask (ask a 'location) 'name)) (symbol->string (ask (ask b 'location) 'name))))

(define (sort list predicate)
  (define (elements->singletons l)
    (map (lambda (x) (cons x '())) l))
  (define (merge-ordered-lists l1 l2 p)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((p (car l1) (car l2)) (cons (car l1) (merge-ordered-lists (cdr l1) l2 p)))
          (else (cons (car l2) (merge-ordered-lists l1 (cdr l2) p)))))
  (define (pairwise-order l p)
    (if (null? l)
        '()
        (let ((first (car l)))
          (if (null? (cdr l))
              (cons (car l) '())
              (let ((second (cadr l)))
                (cons (merge-ordered-lists first second p) (pairwise-order (cddr l) p)))))))
  (define (inner-sort l p)
    (if (= (length l) 1)
        (car l)
        (inner-sort (pairwise-order l p) p)))
  (inner-sort (elements->singletons list) predicate))

#|
(define (ordered-rooms? a b)
  (cond ((and (eq? a 'room_of_requirements) (member? b '(hospital_wing potions_dungeon headmasters_office trophy_room
          divination_classroom owlery charms_classroom library astronomy_tower
          transfiguration_classroom great_hall kitchen
          kitchen broomstick_cabinet quidditch_pitch greenhouse
          dueling_stage))) #t)
        ((and (eq? a 'hospital_wing) (member? b '(potions_dungeon headmasters_office trophy_room
          divination_classroom owlery charms_classroom library astronomy_tower
          transfiguration_classroom great_hall kitchen
          kitchen broomstick_cabinet quidditch_pitch greenhouse
          dueling_stage))) #t)
        ((and (eq? a 'potions_dungeon) (member? b '(headmasters_office trophy_room
          divination_classroom owlery charms_classroom library astronomy_tower
          transfiguration_classroom great_hall kitchen
          kitchen broomstick_cabinet quidditch_pitch greenhouse
          dueling_stage))) #t)
        ((and (eq? a 'headmasters_office) (member? b '(trophy_room
          divination_classroom owlery charms_classroom library astronomy_tower
          transfiguration_classroom great_hall kitchen
          kitchen broomstick_cabinet quidditch_pitch greenhouse
          dueling_stage))) #t)
        ((and (eq? a 'trophy_room) (member? b '(divination_classroom owlery charms_classroom library astronomy_tower
          transfiguration_classroom great_hall kitchen
          kitchen broomstick_cabinet quidditch_pitch greenhouse
          dueling_stage))) #t)
        ((and (eq? a 'divination_classroom) (member? b '(owlery charms_classroom library astronomy_tower
          transfiguration_classroom great_hall kitchen
          kitchen broomstick_cabinet quidditch_pitch greenhouse
          dueling_stage))) #t)
        ((and (eq? a 'owlery) (member? b '(charms_classroom library astronomy_tower
          transfiguration_classroom great_hall kitchen
          kitchen broomstick_cabinet quidditch_pitch greenhouse
          dueling_stage))) #t)
    ((and (eq? a 'charms_classroom) (member? b '(library astronomy_tower
          transfiguration_classroom great_hall kitchen
          kitchen broomstick_cabinet quidditch_pitch greenhouse
          dueling_stage))) #t)
    ((and (eq? a 'library) (member? b '(astronomy_tower
          transfiguration_classroom great_hall kitchen
          kitchen broomstick_cabinet quidditch_pitch greenhouse
          dueling_stage))) #t)
    ((and (eq? a 'astronomy_tower) (member? b '(transfiguration_classroom great_hall kitchen
          kitchen broomstick_cabinet quidditch_pitch greenhouse
          dueling_stage))) #t)
    |#

(define clock (create-clock))

(define (current-time)
  (ask clock 'THE-TIME))

(define (run-clock n)
  (cond ((= n 0) 'DONE)
        (else (ask clock 'tick)
              (run-clock (- n 1)))))


;;-----------
;; screen
;;

(define (a-screen self)
  (let ((deity-mode #t)
        (network-mode #f)
        (me #f)
        (root-part (root-object self)))
    (make-handler
     'screen
     (make-methods
      'TYPE   (lambda () (type-extend 'screen root-part))
      'NAME   (lambda () 'THE-SCREEN)
      'SET-ME (lambda (new-me) (set! me new-me))
      'TELL-ROOM    (lambda (room msg)
                      (if (or deity-mode
                              (eq? room (safe-ask #f me 'location)))
                          (if network-mode
                              (display-net-message msg)
                              (display-message msg))))
      'TELL-WORLD   (lambda (msg)
                      (if network-mode
                          (display-net-message msg)
                          (display-message msg)))
      'DEITY-MODE   (lambda (value) (set! deity-mode value))
      'NETWORK-MODE (lambda (value) (set! network-mode value))
      'DEITY-MODE?  (lambda () deity-mode))
     root-part)))

(define screen
  (create-instance a-screen))

;;--------------------
;; Utilities for our simulation world 
;;

(define (display-message list-of-stuff)
  (if (not (null? list-of-stuff)) (newline))
  (for-each (lambda (s) (display s) (display " "))
            list-of-stuff)
  'MESSAGE-DISPLAYED)

(define (display-net-message list-of-stuff)
  (for-each (lambda (s) (display s) (display " "))
            list-of-stuff)
  (newline)
  (flush-output)
  'MESSAGE-DISPLAYED)

(define (thing-named name)
  (let* ((place (ask me 'LOCATION))
         (things (ask place 'THINGS))
         (peek-stuff (ask me 'PEEK-AROUND))
         (my-stuff (ask me 'THINGS))
         (all-things (append things (append my-stuff peek-stuff)))
         (things-named (filter (lambda (x) (eq? name (ask x 'NAME)))
                               all-things)))
    (cond ((null? things-named)
           (error "In here there is nothing named" name))
          ((null? (cdr things-named))
           (car things-named))
          (else
           (display-message (list "There is more than one thing named"
                                  name "here. Picking one of them."))
           (pick-random things-named)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; OBJECT TYPES
;;;
;;;

;;--------------------
;; named-object
;; 

(define (create-named-object name)
  (create-instance named-object name))

(define (named-object self name)
  (let ((root-part (root-object self)))
    (make-handler
     'named-object
     (make-methods
      'NAME    (lambda () name)
      'INSTALL (lambda () 'installed)
      'DESTROY (lambda () 'destroyed))
     root-part)))

(define (names-of objects)
  (map (lambda (x) (ask x 'NAME)) objects))


;;--------------------
;; container
;;

(define (member? x list)
  (if (member x list) #t #f))

(define (container self)
  (let ((root-part (root-object self))
        (things '()))
    (make-handler
     'container
     (make-methods
      'THINGS      (lambda () things)
      'HAVE-THING? (lambda (thing)
                     (member? thing things))
      'ADD-THING   (lambda (thing)
                     (if (not (ask self 'HAVE-THING? thing))
                         (set! things (cons thing things)))
                     'DONE)
      'DEL-THING   (lambda (thing)
                     (set! things (delq thing things))
                     'DONE))
     root-part)))

;;--------------------
;; thing
;;

(define (create-thing name location)
  (create-instance thing name location))

(define (thing self name location)
  (let ((named-part (named-object self name)))
    (make-handler
     'thing
     (make-methods
      'INSTALL  (lambda ()
                  (ask named-part 'INSTALL)
                  (ask (ask self 'LOCATION) 'ADD-THING self))
      'LOCATION (lambda () location)
      'DESTROY  (lambda ()
                  (ask (ask self 'LOCATION) 'DEL-THING self))
      'EMIT     (lambda (text)
                  (ask screen 'TELL-ROOM (ask self 'LOCATION)
                       (append (list "At" (ask (ask self 'LOCATION) 'NAME))
                               text))))
     named-part)))

;;--------------------
;; mobile-thing
;;

(define (create-mobile-thing name location) 
  (create-instance mobile-thing name location))

(define (mobile-thing self name location)
  (let ((thing-part (thing self name location)))
    (make-handler
     'mobile-thing
     (make-methods
      'LOCATION  (lambda () location)
      'CHANGE-LOCATION
      (lambda (new-location)
        (ask location 'DEL-THING self)
        (ask new-location 'ADD-THING self)
        (set! location new-location))
      'ENTER-ROOM    (lambda () #t)
      'LEAVE-ROOM    (lambda () #t)
      'CREATION-SITE (lambda () (ask thing-part 'location)))
     thing-part)))

;;--------------------
;; place
;;

(define (create-place name)
  (create-instance place name))

(define (place self name)
  (let ((named-part (named-object self name))
        (container-part (container self))
        (exits '()))
    (make-handler
     'place
     (make-methods
      'EXITS (lambda () exits)
      'EXIT-TOWARDS
      (lambda (direction)
        (find-exit-in-direction exits direction))
      'ADD-EXIT
      (lambda (exit)
        (let ((direction (ask exit 'DIRECTION)))
          (if (ask self 'EXIT-TOWARDS direction)
              (error (list name "already has exit" direction))
              (set! exits (cons exit exits)))
          'DONE)))
     container-part named-part)))


;;------------------------
;;clasroom
;;

(define (create-classroom name)
  (create-instance classroom name))

(define (classroom self name)
  (let ((place-part (place self name)))
    (make-handler
     'classroom
     (make-methods
      'TRY-TO-TEACH
      (lambda (student)
        (let ((spell (pick-random (ask chamber-of-secrets 'THINGS))))
          (if (= (random 2) 0)
              (begin (clone-spell-or-counterspell spell self)
                     (ask student 'SAY (list "I've just learned" (ask spell 'NAME))))))))
     place-part)))
            

;;------------------------------------------------------------
;; exit
;;

(define (create-exit from direction to)
  (create-instance exit from direction to))

(define (exit self from direction to)
  (let ((named-object-part (named-object self direction)))
    (make-handler
     'exit
     (make-methods
      'INSTALL
      (lambda ()
        (ask named-object-part 'INSTALL)
        (if (not (null? (ask self 'FROM)))
            (ask (ask self 'FROM) 'ADD-EXIT self)))
      'FROM         (lambda () from)
      'TO           (lambda () to)
      'DIRECTION    (lambda () direction)
      'USE
      (lambda (whom)
        (ask whom 'LEAVE-ROOM)
        (ask screen 'TELL-ROOM (ask whom 'location)
             (list (ask whom 'NAME)
                   "moves from" 
                   (ask (ask whom 'LOCATION) 'NAME)
                   "to"
                   (ask to 'NAME)))
        (ask whom 'CHANGE-LOCATION to)
        (ask whom 'ENTER-ROOM)))
     named-object-part)))

(define (find-exit-in-direction exits dir)
  (cond ((null? exits) #f)
        ((eq? dir (ask (car exits) 'DIRECTION))
         (car exits))
        (else (find-exit-in-direction (cdr exits) dir))))

(define (random-exit place)
  (pick-random (ask place 'EXITS)))

;;--------------------
;; person
;;

(define (create-person name birthplace)
  (create-instance person name birthplace))

(define (person self name birthplace)
  (let ((mobile-thing-part (mobile-thing self name birthplace))
        (container-part    (container self))
        (health            3)
        (strength          1))
    (make-handler 'person
                  (make-methods
                   'STRENGTH (lambda () strength)
                   'HEALTH (lambda () health)
                   'SAY
                   (lambda (list-of-stuff)
                     (ask screen 'TELL-ROOM (ask self 'location)
                          (append (list "At" (ask (ask self 'LOCATION) 'NAME)
                                        (ask self 'NAME) "says --")
                                  list-of-stuff))
                     'SAID-AND-HEARD)
                   'HAVE-FIT
                   (lambda ()
                     (ask self 'SAY '("Yaaaah! I am upset!"))
                     'I-feel-better-now)
                   
                   'PEOPLE-AROUND
                   (lambda ()
                     (filter (lambda (x) (map (lambda (y) (member? 'ring-of-obfuscation (ask y 'TYPE)))
                                              (ask x 'THINGS)))
                             (delq self (find-all (ask self 'LOCATION) 'PERSON))))
                   
                   'STUFF-AROUND
                   (lambda ()
                     (let* ((in-room (ask (ask self 'LOCATION) 'THINGS))
                            (stuff (filter (lambda (x) (not (ask x 'IS-A 'PERSON))) in-room)))
                       stuff))
                   
                   'PEEK-AROUND
                   (lambda ()
                     (let ((people (ask self 'PEOPLE-AROUND)))
                       (fold-right append '() (map (lambda (p) (ask p 'THINGS)) people))))
                   
                   'TAKE
                   (lambda (thing)
                     (cond ((ask self 'HAVE-THING? thing)
                            (ask self 'SAY (list "I am already carrying"
                                                 (ask thing 'NAME)))
                            #f)
                           ((or (ask thing 'IS-A 'PERSON)
                                (not (ask thing 'IS-A 'MOBILE-THING)))
                            (ask self 'SAY (list "I try but cannot take"
                                                 (ask thing 'NAME)))
                            #f)
                           (else
                            (let ((owner (ask thing 'LOCATION)))
                              (ask self 'SAY (list "I take" (ask thing 'NAME) 
                                                   "from" (ask owner 'NAME)))
                              (if (ask owner 'IS-A 'PERSON)
                                  (ask owner 'LOSE thing self)
                                  (ask thing 'CHANGE-LOCATION self))
                              thing))))
                   
                   'LOSE
                   (lambda (thing lose-to)
                     (ask self 'SAY (list "I lose" (ask thing 'NAME)))
                     (ask self 'HAVE-FIT)
                     (ask thing 'CHANGE-LOCATION lose-to))
                   
                   'DROP
                   (lambda (thing)
                     (ask self 'SAY (list "I drop" (ask thing 'NAME)
                                          "at" (ask (ask self 'LOCATION) 'NAME)))
                     (ask thing 'CHANGE-LOCATION (ask self 'LOCATION)))
                   
                   'GO-EXIT
                   (lambda (exit)
                     (ask exit 'USE self))
                   
                   'GO
                   (lambda (direction)
                     (let ((exit (ask (ask self 'LOCATION) 'EXIT-TOWARDS direction)))
                       (if (and exit (ask exit 'IS-A 'EXIT))
                           (ask self 'GO-EXIT exit)
                           (begin (ask screen 'TELL-ROOM (ask self 'LOCATION)
                                       (list "No exit in" direction "direction"))
                                  #F))))
                   'SUFFER
                   (lambda (hits perp)
                     (ask self 'SAY (list "Ouch!" hits "hits is more than I want!"))
                     (set! health (- health hits))
                     (if (<= health 0) (ask self 'DIE perp))
                     health)
                   
                   'DIE
                   (lambda (perp)
                     (for-each (lambda (item) (ask self 'LOSE item (ask self 'LOCATION)))
                               (ask self 'THINGS))
                     (ask screen 'TELL-WORLD
                          '("An earth-shattering, soul-piercing scream is heard..."))
                     (ask self 'DESTROY))
                   
                   'ENTER-ROOM
                   (lambda ()
                     (let* ((others (ask self 'PEOPLE-AROUND))
                            (professors (filter (lambda (x) (ask x 'IS-A 'wit-professor)) others))
                            (students (filter (lambda (x) (and (ask x 'IS-A 'wit-students)
                                                               (not (ask x 'IS-A 'wit-professor)))) others))
                            (monitors (filter (lambda (x) (ask x 'IS-A 'hall-monitor)) others)))
                       (if (not (null? others))
                           (begin (if (not (null? professors))
                                      (ask self 'SAY (cons "Good day," (names-of professors))))
                                  (if (not (null? monitors))
                                      (ask self 'SAY (cons "Hello," (names-of monitors))))
                                  (if (not (null? students))
                                      (ask self 'SAY (cons "Hi," (names-of students)))))))
                     #T)
                   
                   'HAS-A
                   (lambda (type)
                     (let* ((things (ask self 'THINGS))
                            (wanted-things (filter (lambda (x) (member? type (ask x 'TYPE))) things)))
                       (if (not (null? wanted-things))
                           (begin (ask self 'SAY (list "I have a" type))
                                  #t))))
                   
                   'HAS-A-THING-NAMED
                   (lambda (name)
                     (let* ((things (ask self 'THINGS))
                            (the-thing (filter (lambda (x) (eq? (ask x 'NAME) name)) things)))
                       (if (null? the-thing)
                           (begin (ask self 'SAY (list "I haven't got the " name)) #f)
                           (begin (ask self 'SAY (list "I've got the " name)) #t))))
                   
                   'RETRIEVE
                   (lambda (name)
                     (if (ask self 'HAS-A-THING-NAMED name)
                         (pick-random (filter (lambda (x) (eq? (ask x 'NAME) name)) (ask self 'THINGS)))
                         (begin (ask self 'EMIT (list "I haven't got the" name))
                                #f))))
                  mobile-thing-part container-part)))

;;--------------------
;; autonomous-person
;;

(define (create-autonomous-person name birthplace activity miserly)
  (create-instance autonomous-person name birthplace activity miserly))

(define (autonomous-person self name birthplace activity miserly)
  (let ((person-part (person self name birthplace)))
    (make-handler
     'autonomous-person
     (make-methods
      'INSTALL
      (lambda ()
        (ask person-part 'INSTALL)
        (ask clock 'ADD-CALLBACK
             (create-clock-callback 'move-somewhere self 
                                    'MOVE-RANDOMLY))
        (ask clock 'ADD-CALLBACK
             (create-clock-callback 'take-something self
                                    'TAKE-RANDOMLY)))
      
      ;'MOVE-AND-TAKE-STUFF
      ;(lambda ()
      ;  (let loop ((moves (random-number activity)))
      ;    (if (= moves 0)
      ;        'done-moving
      ;        (begin
      ;          (ask self 'MOVE-SOMEWHERE)
      ;          (loop (- moves 1)))))
      ;  (if (= (random miserly) 0)
      ;      (ask self 'TAKE-SOMETHING))
      ;  'done-for-this-tick)
      
      'DIE
      (lambda (perp)
        (ask clock 'REMOVE-CALLBACK self 'move-somewhere)
        (ask clock 'REMOVE-CALLBACK self 'take-something)
        (ask self 'SAY '("SHREEEEK!  I, uh, suddenly feel very faint..."))
        (ask person-part 'DIE perp))
      
      'MOVE-SOMEWHERE
      (lambda ()
        (let ((exit (random-exit (ask self 'LOCATION))))
          (if (not (null? exit)) (ask self 'GO-EXIT exit))))
      
      'MOVE-RANDOMLY
      (lambda ()
        (let loop ((moves (random-number activity)))
          (if (= moves 0)
              'done-moving
              (begin
                (ask self 'MOVE-SOMEWHERE)
                (loop (- moves 1))))))
      
      'TAKE-SOMETHING
      (lambda ()
        (let* ((stuff-in-room (ask self 'STUFF-AROUND))
               (other-peoples-stuff (ask self 'PEEK-AROUND))
               (pick-from (append stuff-in-room other-peoples-stuff)))
          (if (not (null? pick-from))
              (ask self 'TAKE (pick-random pick-from))
              #F)))
      
      'TAKE-RANDOMLY
      (lambda ()
        (if (= (random miserly) 0)
            (ask self 'TAKE-SOMETHING))))
     person-part)))

;;--------------------
;; wit-student
;;

(define (create-wit-student name birthplace activity miserly)
  (create-instance wit-student name birthplace activity miserly))

(define (wit-student self name birthplace activity miserly)
  (let ((autonomous-person-part (autonomous-person self name birthplace activity miserly)))
    (make-handler
     'wit-student
     (make-methods
      'INSTALL
      (lambda ()
        (ask autonomous-person-part 'INSTALL)
        (ask clock 'ADD-CALLBACK
             (create-clock-callback 'zap-someone self 
                                    'ZAP-SOMEONE))
        (ask clock 'ADD-CALLBACK
             (create-clock-callback 'learn self 'LEARN)))
      
      'ZAP-SOMEONE
      (lambda ()
        (let ((target (pick-random (ask self 'PEOPlE-AROUND)))
              (wand (pick-random (filter (lambda (x) (ask x 'IS-A 'wand))
                                         (ask self 'THINGS)))))
          (cond ((and target wand) (ask wand 'ZAP target))
                ((and (not target) wand) (ask wand 'WAVE)))))
        
      'LEARN
        (lambda ()
          (let ((room (ask self 'LOCATION)))
          (if (ask room 'IS-A 'classroom)
              (ask room 'TRY-TO-TEACH self))))
      
      'DIE
      (lambda (perp)
        (ask clock 'REMOVE-CALLBACK self 'zap-someone)
        (ask autonomous-person-part 'DIE perp)))
    autonomous-person-part)))


;;--------------------
;; chosen-one
;;

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


;;--------------------
;; professor
;;

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
                                          (ask self 'PEOPLE-AROUND))))
              (location (ask self 'LOCATION)))
          (if (ask location 'IS-A 'classroom)
              (if pupil
                  (let ((spell (pick-random (ask chamber-of-secrets 'THINGS))))
                    (begin (clone-spell-or-counterspell spell pupil)
                           (ask self 'EMIT (list (ask self 'NAME) "teaches" (ask spell 'NAME)
                                                 "to" (ask pupil 'NAME)))))))))
      
      'DIE
      (lambda (perp)
        (ask clock 'REMOVE-CALLBACK self 'teach-a-spell)
        (ask student-part 'DIE perp)))
     student-part)))


;;--------------------
;; hall-monitor
;;

(define (create-hall-monitor name birthplace speed irritability)
  (create-instance hall-monitor name birthplace speed irritability))

(define (hall-monitor self name birthplace speed irritability)
  (let ((auto-part (autonomous-person self name birthplace speed 10)))
    (make-handler
     'hall-monitor
     (make-methods
      'INSTALL
      (lambda ()
        (ask auto-part 'INSTALL)
        (ask clock 'ADD-CALLBACK
             (create-clock-callback 'irritate-students self
                                    'IRRITATE-STUDENTS)))
      'IRRITATE-STUDENTS
      (lambda ()
        (if (= (random irritability) 0)
            (let ((people (ask self 'PEOPLE-AROUND)))
              (if (not (null? people))
                  (begin
                    (ask self 'SAY '("What are you doing still up?"
                                     "Everyone back to their rooms!"))
                    (for-each (lambda (person)
                                (ask person 'EMIT 
                                     (list (ask person 'NAME) "goes home to"
                                           (ask (ask person 'CREATION-SITE) 'NAME)))
                                (ask person 'CHANGE-LOCATION
                                     (ask person 'CREATION-SITE)))
                              people)
                    'grumped)
                  (ask self 'SAY '("Grrr... When I catch those students..."))))
            (if (ask self 'PEOPLE-AROUND)
                (ask self 'SAY '("I'll let you off this once...")))))
      'DIE
      (lambda (perp)
        (ask clock 'REMOVE-CALLBACK self 'irritate-students)
        (ask auto-part 'DIE perp)))
     auto-part)))

;;--------------------
;; troll
;;

(define (create-troll name birthplace speed hunger)
  (create-instance troll name birthplace speed hunger))

(define (troll self name birthplace speed hunger)
  (let ((auto-part (autonomous-person self name birthplace speed 10)))
    (make-handler
     'troll
     (make-methods
      'INSTALL
      (lambda ()
        (ask auto-part 'INSTALL)
        (ask clock 'ADD-CALLBACK
             (create-clock-callback 'eat-people self
                                    'EAT-PEOPLE)))
      'EAT-PEOPLE
      (lambda ()
        (if (= (random hunger) 0)
            (let ((people (ask self 'PEOPLE-AROUND)))
              (if (not (null? people))
                  (let ((victim (pick-random people)))
                    (ask self 'EMIT
                         (list (ask self 'NAME) "takes a bite out of"
                               (ask victim 'NAME)))
                    (ask victim 'SUFFER (random-number 3) self)
                    'tasty)
                  (ask self 'EMIT
                       (list (ask self 'NAME) "'s belly rumbles"))))
            'not-hungry-now))
      'DIE
      (lambda (perp)
        (ask clock 'REMOVE-CALLBACK self 'eat-people)
        (ask auto-part 'DIE perp)))
     auto-part)))

;;--------------------
;; spell
;;

(define (create-spell name location incant action counterspell)
  (create-instance spell name location incant action counterspell))

(define (spell self name location incant action counterspell)
  (let ((mobile-part (mobile-thing self name location)))
    (make-handler
     'spell
     (make-methods
      'INCANT
      (lambda () incant)
      'ACTION
      (lambda () action)
      'COUNTERSPELL
      (lambda () counterspell)
      'USE
      (lambda (caster target)
        (let ((counter (ask self 'COUNTERSPELL)))
          (if (ask target 'HAS-A-THING-NAMED counter)
              (ask (ask target 'RETRIEVE counter) 'COUNTERACT self)
              (action caster target)))))
     mobile-part)))


;;--------------------
;; counterspell
;;

(define (create-counterspell name location incant spell)
  (create-instance counterspell name location incant spell))

(define (counterspell self name location incant spell)
  (let ((mobile-part (mobile-thing self name location)))
    (make-handler
     'counterspell
     (make-methods
      'INCANT
      (lambda () incant)
      'SPELL
      (lambda () spell)
      'COUNTERACT
      (lambda (perp)
        (ask (ask self 'LOCATION) 'SAY (list "I counter your" (ask perp 'NAME)))))
     mobile-part)))

(define (clone-spell-or-counterspell spell newloc)
  (if (ask spell 'IS-A 'spell)
      (begin (create-spell (ask spell 'NAME)
                newloc
                (ask spell 'INCANT)
                (ask spell 'ACTION)
                (ask spell 'COUNTERSPELL)))
      (begin (create-counterspell (ask spell 'NAME)
                                  newloc
                                  (ask spell 'INCANT)
                                  (ask spell 'SPELL)))))
                             

;;--------------------
;; avatar
;;

(define (create-avatar name birthplace)
  (create-instance avatar name birthplace))

(define (avatar self name birthplace)
  (let ((person-part (person self name birthplace)))
    (make-handler
     'avatar
     (make-methods
      'LOOK-AROUND
      (lambda ()
        (let* ((place (ask self 'LOCATION))
               (exits (ask place 'EXITS))
               (other-people (ask self 'PEOPLE-AROUND))
               (my-stuff (ask self 'THINGS))
               (stuff (ask self 'STUFF-AROUND)))
          (ask screen 'TELL-WORLD (list "You are in" (ask place 'NAME)))
          (ask screen 'TELL-WORLD
               (if (null? my-stuff)
                   '("You are not holding anything.")
                   (append '("You are holding:") (names-of my-stuff))))
          (ask screen 'TELL-WORLD
               (if (null? stuff)
                   '("There is no stuff in the room.")
                   (append '("You see stuff in the room:") (names-of stuff))))
          (ask screen 'TELL-WORLD
               (if (null? other-people)
                   '("There are no other people around you.")
                   (append '("You see other people:") (names-of other-people))))
          (ask screen 'TELL-WORLD
               (if (not (null? exits))
                   (append '("The exits are in directions:") (names-of exits))
                   '("There are no exits... you are dead and gone to heaven!")))
          'OK))
      
      'GO
      (lambda (direction)
        (let ((success? (ask person-part 'GO direction)))
          (if success? (ask clock 'TICK))
          success?))
      
      'DIE
      (lambda (perp)
        (ask self 'SAY (list "I am slain!"))
        (ask person-part 'DIE perp))
      
      'FEEL-THE-FORCE
      (lambda ()
        (for-each (lambda (x) (ask screen 'TELL-WORLD (list (ask x 'NAME) "is at" (ask (ask x 'LOCATION) 'NAME))))
                  (filter (lambda (x) (map (lambda (y) (member? 'ring-of-obfuscation (ask y 'TYPE)))
                                           (ask x 'THINGS)))
                          (all-people))))
      
      'CHOSE-OBJECT
      (lambda (name)
        (let ((object (filter (lambda (x) (eq? (ask x 'NAME) name)) (ask self 'THINGS))))
          (if (null? object)
              (ask self 'EMIT (list "I haven't got the" name))
              (car object)))))
     
     person-part)))

;;--------------------
;; ring-of-obfuscation
;;

(define (create-ring name location)
  (create-instance ring-of-obfuscation name location))

(define (ring-of-obfuscation self name location)
  (let ((mobile-thing-part (mobile-thing self name location)))
    (make-handler
     'ring-of-obfuscation
     (list 'methods)
     mobile-thing-part)))

;;--------------------
;; wand
;;

(define (create-wand name location)
  (create-instance wand name location))

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
            (if (not (null? spells))
                (let ((spell (pick-random spells)))
                  (begin (ask owner 'SAY (list "I am zapping a wand at" (ask target 'NAME)
                                                ", saying " (ask spell 'INCANT)))
                         (apply (ask spell 'ACTION) (list owner target))))))))
      'WAVE
      (lambda ()
        (let ((owner (ask self 'LOCATION)))
          (if (not (member? 'person (ask owner 'TYPE)))            
              (ask self 'EMIT (list "Wands can only be used by people!")))
          (let ((things (filter (lambda (x) (not (or (ask x 'IS-A 'spell) (ask x 'IS-A 'counterspell))))
                                (ask owner 'STUFF-AROUND))))
            (if (not (null? things))
                (ask self 'ZAP (pick-random things)))))))
     mobile-thing-part)))


;;--------------------
;; owl
;;

(define (owl self name owner location)
  (let ((mobile-thing-part (mobile-thing self name location)))
    (make-handler
     'owl
     (make-methods
      'INSTALL
      (lambda ()
        (ask mobilie-thing-part 'INSTALL)
        (ask clock 'ASS-CALLBACK (create-clock-callback 'attempt-delivering self
                                                        'ATTEMPT-DELIVERING)))
      
      'ATTEMPT-DELIVERING
      (lambda ()
        )
      
      'DELIVER-A-MESSAGE
      (lambda (message recepient)
        
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; SETUP
;;;


;;------------------------------------------------------------
;; Utils to connect places by way of exits

(define (can-go-both-ways from direction reverse-direction to)
  (create-exit from direction to)
  (create-exit to reverse-direction from))

;;------------------------------------------------------------
;; Create our world...

(define (create-world)
  (let ((charms_classroom (create-classroom 'charms_classroom))
        (potions_dungeon (create-classroom 'potions_dungeon))
        (great_hall (create-place 'great_hall))
        (library (create-classroom 'library))
        (forbidden_section (create-place 'forbidden_section))
        (greenhouse (create-place 'greenhouse))
        (hospital_wing (create-place 'hospital_wing))
        (headmasters_office (create-place 'headmasters_office))
        (astronomy_tower (create-place 'astronomy_tower))
        (quidditch_pitch (create-place 'quidditch_pitch))
        (divination_classroom (create-classroom 'divination_classroom))
        (owlery (create-place 'owlery))
        (transfiguration_classroom (create-classroom 'transfiguration_classroom))
        (kitchen (create-place 'kitchen))
        (broomstick_cabinet (create-place 'broomstick_cabinet))
        (trophy_room (create-place 'trophy_room))
        (room_of_requirements (create-place 'room_of_requirements))
        (dueling_stage (create-place 'dueling-stage)))
    
    (can-go-both-ways hospital_wing 'up 'down room_of_requirements)
    (can-go-both-ways potions_dungeon 'up 'down hospital_wing)
    (can-go-both-ways room_of_requirements 'up 'down headmasters_office)
    (can-go-both-ways hospital_wing 'west 'east trophy_room)
    (can-go-both-ways trophy_room 'west 'east kitchen)
    (can-go-both-ways kitchen 'south 'north broomstick_cabinet)
    (can-go-both-ways broomstick_cabinet 'west 'east quidditch_pitch)
    (can-go-both-ways hospital_wing 'north 'south transfiguration_classroom)
    (can-go-both-ways hospital_wing 'south 'north great_hall)
    (can-go-both-ways transfiguration_classroom 'north 'south charms_classroom)
    (can-go-both-ways charms_classroom 'up 'down library)
    (can-go-both-ways library 'up 'down divination_classroom)
    (can-go-both-ways library 'east 'west forbidden_section)
    (can-go-both-ways forbidden_section 'north 'south forbidden_section)
    (can-go-both-ways forbidden_section 'up 'down forbidden_section)
    (can-go-both-ways divination_classroom 'west 'east owlery)
    (can-go-both-ways charms_classroom 'north 'south greenhouse)
    (can-go-both-ways divination_classroom 'up 'down astronomy_tower)
    (can-go-both-ways greenhouse 'east 'west great_hall)
    (can-go-both-ways great_hall 'up 'down dueling_stage)
    
    (create-thing 'hammock room_of_requirements)
    (create-thing 'head_table great_hall)
    (create-thing 'telescope astronomy_tower)
    (create-thing 'forbidden_charms forbidden_section)
    (create-thing 'cauldron potions_dungeon)
    (create-mobile-thing 'owl owlery)
    (create-mobile-thing 'crystal_ball divination_classroom)
    (create-mobile-thing 'broomstick broomstick_cabinet)
    (create-mobile-thing 'prefect-tablo trophy_room)
    (create-mobile-thing 'phoenix headmasters_office)
    (create-mobile-thing 'trickle-tart great_hall)
    (create-mobile-thing 'history_of_magical_britain library)
    (create-mobile-thing 'mandragora greenhouse)
    
    (list room_of_requirements hospital_wing potions_dungeon headmasters_office trophy_room
          divination_classroom owlery charms_classroom library astronomy_tower
          transfiguration_classroom great_hall kitchen
          kitchen broomstick_cabinet quidditch_pitch greenhouse
          dueling_stage)))


(define (instantiate-spells-and-counterspells)
  (let ((chamber (create-place 'chamber-of-secrets)))
    (create-spell
     'boil-spell
     chamber
     "habooic katarnum"
     (lambda (caster target)
       (if (ask target 'IS-A 'person)
           (ask screen 'TELL-WORLD (list (ask target 'NAME) "grows boils on their nose."))
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
           (begin (ask screen 'TELL-WORLD (list "A slug comes out of" (ask target 'NAME) "'s mouth."))
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
       (ask screen 'TELL-WORLD (list "Let the wind of doom blow!"))
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

(define (populate-spells-and-counterspells rooms)
  (for-each (lambda (room)
              (begin (clone-spell-or-counterspell (pick-random (ask chamber-of-secrets 'THINGS)) room)
                     (clone-spell-or-counterspell (pick-random (ask chamber-of-secrets 'THINGS)) room)))
            rooms))

(define (populate-players rooms)
  (let* ((students (map (lambda (name) (create-wit-student name (pick-random rooms)
                                                           (random-number 3) (random-number 3)))
                        '(Ben Alyssa Lem Cide)))
         (students-wands (map (lambda (person) (create-wand
                                                (pick-random '(unicorn_hair_wand vampire_tooth_wand
                                                               hydra_eye_wand mermaid_scale_wand
                                                               centaur_heart_wand dragon_blood_wand))
                                                person))
                              students))
                         
         	 (profs (map (lambda (name)
         		       (create-wit-professor name
         					     (pick-random rooms)
         					     (random-number 3)
         					     (random-number 3)))
         		     '(Prof_Flitwick Prof_McGonagall)))
         (monitors (map (lambda (name)
                          (create-hall-monitor name
                                               (pick-random rooms)
                                               (random-number 3)
                                               (random-number 3)))
                        '(Filtch Mrs.Norris)))
         (trolls (map (lambda (name)
                        (create-troll name
                                      (pick-random rooms)
                                      (random-number 3)
                                      (random-number 3)))
                      '(Urdo Razdo))))
    
    (append students
            profs
            monitors
            trolls)))

(define (populate-rings rooms)
  (for-each (lambda (room)
              (if (= (random 8) 0)
                  (ask room 'add-thing (create-ring 'ring room))))
            rooms))

(define (populate-wands rooms)
  (map (lambda (wand-type) (create-wand wand-type (pick-random rooms)))
       '(unicorn_hair_wand vampire_tooth_wand
                           hydra_eye_wand mermaid_scale_wand
                           centaur_heart_wand dragon_blood_wand)))

(define me 'will-be-set-by-setup)
(define all-rooms 'will-be-set-by-setup)
(define chamber-of-secrets 'will-be-set-by-setup)


(define (all-people)
  (append-map (lambda (room) (find-all room 'person)) all-rooms)) 

(define (setup name)
  (ask clock 'RESET)
  (ask clock 'ADD-CALLBACK
       (create-clock-callback 'tick-printer clock 'PRINT-TICK))
  (let ((rooms (create-world)))
    (set! chamber-of-secrets (instantiate-spells-and-counterspells))
    
    (populate-spells-and-counterspells rooms)
    
    (populate-players rooms)
    
    (populate-rings rooms)
    
    (populate-wands rooms)
    
    (create-chosen-one 'Hairy_Cdr (pick-random rooms)
    	              (random-number 3) (random-number 3))
    
    (set! me (create-avatar name (pick-random rooms)))
    (ask screen 'SET-ME me)
    (set! all-rooms rooms)
    'ready))



(setup 'ida)
(run-clock 10)



