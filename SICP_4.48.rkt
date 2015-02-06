#lang racket

;Exercise 4.48

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))
(define connectives '(con and but because so ))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found word)))


(define (parse-sentence)
  (amb (list 'sentence (parse-noun-phrase) (pars-verb-phrase))
       (parse-coumpound-sentence)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

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

(define (parse-compound-sentence)
  (list 'compound-sentence
        (parse-sentence)
        (parse-word connectives)
        (parse sentence)))