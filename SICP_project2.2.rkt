#lang racket

;; A sampler of 2-player strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN  my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
          ((string=? (most-recent-play hist) test)
           (+ (count-instances-of test (rest-of-plays hist)) 1))
          (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
        (cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))
;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;	    

(define *game-association-list*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)


;; Problem 9
(define (play-loop strat0 strat1 strat2)
  (define (play-loop-iter strat0 strat1 strat2 count history0 history1 history2 limit)
    (cond ((= count limit) (print-out-results history0 history1 history2 limit))
          (else (let ((result0 (strat0 history0 history1 history2))
                      (result1 (strat1 history1 history0 history2))
                      (result2 (strat2 history2 history0 history1)))
                  (play-loop-iter strat0 strat1 strat2 (+ count 1)
                                  (extend-history result0 history0)
                                  (extend-history result1 history1)
                                  (extend-history result2 history2)
                                  limit)))))
  (play-loop-iter strat0 strat1 strat2 0
                  the-empty-history the-empty-history the-empty-history
                  (+ 90 (random 21))))


(define (print-out-results history0 history1 history2 number-of-games)
  (let ((scores (get-scores history0 history1 history2)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score:  ")
    (display (* 1.0 (/ (caddr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1 history2)
  (define (get-scores-helper history0 history1 history2 score0 score1 score2)
    (cond ((empty-history? history0) (list score0 score1 score2))
          (else (let ((game (make-play (most-recent-play history0)
                                       (most-recent-play history1)
                                       (most-recent-play history2))))
                  (get-scores-helper (rest-of-plays history0)
                                     (rest-of-plays history1)
                                     (rest-of-plays history2)
                                     (+ (get-player-points 0 game) score0)
                                     (+ (get-player-points 1 game) score1)
                                     (+ (get-player-points 2 game) score2))))))
  (get-scores-helper history0 history1 history2 0 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

(define (extract-entry game matrix)
  (cond ((null? matrix) (error "No results listed for the game:" game))
        ((equal? game (caar matrix)) (car matrix))
        (else (extract-entry game (cdr matrix)))))

(define test-extract-entry
  (let ((a-play (make-play "c" "c" "c")))
    (extract-entry a-play *game-association-list*)))

;; Problem 10
(define (PATSY-3 my-history other1-history other2-history)
  "c")

(define (NASTY-3 my-history other1-history other2-history)
  "d")

(define (SPASTIC-3 my-history other1-history other2-history)
  (if (= (random 2) 0)
      "c"
      "d"))

;(play-loop PATSY-3 NASTY-3 SPASTIC-3)

(define (TOUGH-EYE-FOR-EYE my-history other1-history other2-history)
  (cond ((empty-history? my-history) "c")
        ((or (string=? (most-recent-play other1-history) "d")
             (string=? (most-recent-play other2-history) "d"))
         "d")
        (else "c")))

(define (SOFT-EYE-FOR-EYE my-history other1-history other2-history)
  (cond ((empty-history? my-history) "c")
        ((and (string=? (most-recent-play other1-history) "d")
              (string=? (most-recent-play other2-history) "d"))
         "d")
        (else "c")))


;; Problem 11
(define (make-combined-strategies strat1 strat2 proc)
  (lambda (my-history other1-history other2-history)
    (let ((result1 (strat1 my-history other1-history))
          (result2 (strat2 my-history other2-history)))
      (proc result1 result2))))

(define COMBINED-EYE-FOR-EYE
  (make-combined-strategies EYE-FOR-EYE
                            EYE-FOR-EYE
                            (lambda (r1 r2) (if (or (string=? r1 "d") (string=? r1 "d"))
                                                "d"
                                                "c"))))

(define COMBINED-EGAL-EYE
  (make-combined-strategies EGALITARIAN
                            EYE-FOR-EYE
                            (lambda (r1 r2) (if (= (random 2) 0) r1 r2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;          3-PLAYER TOURNAMENT
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(define patsy-all-permutations
  (list (play-loop PATSY-3 NASTY-3 SPASTIC-3)
        (play-loop PATSY-3 NASTY-3 TOUGH-EYE-FOR-EYE)
        (play-loop PATSY-3 NASTY-3 SOFT-EYE-FOR-EYE)
        (play-loop PATSY-3 NASTY-3 COMBINED-EGAL-EYE)
        (play-loop PATSY-3 SPASTIC-3 TOUGH-EYE-FOR-EYE)
        (play-loop PATSY-3 SPASTIC-3 SOFT-EYE-FOR-EYE)
        (play-loop PATSY-3 SPASTIC-3 COMBINED-EGAL-EYE)
        (play-loop PATSY-3 TOUGH-EYE-FOR-EYE SOFT-EYE-FOR-EYE)
        (play-loop PATSY-3 TOUGH-EYE-FOR-EYE COMBINED-EGAL-EYE)
        (play-loop PATSY-3 SOFT-EYE-FOR-EYE COMBINED-EGAL-EYE)
        (play-loop PATSY-3 PATSY-3 PATSY-3)
        (play-loop PATSY-3 PATSY-3 NASTY-3)
        (play-loop PATSY-3 PATSY-3 SPASTIC-3)
        (play-loop PATSY-3 PATSY-3 TOUGH-EYE-FOR-EYE)
        (play-loop PATSY-3 PATSY-3 SOFT-EYE-FOR-EYE)
        (play-loop PATSY-3 PATSY-3 COMBINED-EGAL-EYE)
        (play-loop PATSY-3 NASTY-3 NASTY-3)
        (play-loop PATSY-3 SPASTIC-3 SPASTIC-3)
        (play-loop PATSY-3 TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE)
        (play-loop PATSY-3 SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE)
        (play-loop PATSY-3 COMBINED-EGAL-EYE COMBINED-EGAL-EYE)))

(define nasty-all-permutations
  (list (play-loop NASTY-3 PATSY-3 SPASTIC-3)
        (play-loop NASTY-3 PATSY-3 TOUGH-EYE-FOR-EYE)
        (play-loop NASTY-3 PATSY-3 SOFT-EYE-FOR-EYE)
        (play-loop NASTY-3 PATSY-3 COMBINED-EGAL-EYE)
        (play-loop NASTY-3 SPASTIC-3 TOUGH-EYE-FOR-EYE)
        (play-loop NASTY-3 SPASTIC-3 SOFT-EYE-FOR-EYE)
        (play-loop NASTY-3 SPASTIC-3 COMBINED-EGAL-EYE)
        (play-loop NASTY-3 TOUGH-EYE-FOR-EYE SOFT-EYE-FOR-EYE)
        (play-loop NASTY-3 TOUGH-EYE-FOR-EYE COMBINED-EGAL-EYE)
        (play-loop NASTY-3 SOFT-EYE-FOR-EYE COMBINED-EGAL-EYE)
        (play-loop NASTY-3 NASTY-3 NASTY-3)
        (play-loop NASTY-3 NASTY-3 PATSY-3)
        (play-loop NASTY-3 NASTY-3 SPASTIC-3)
        (play-loop NASTY-3 NASTY-3 TOUGH-EYE-FOR-EYE)
        (play-loop NASTY-3 NASTY-3 SOFT-EYE-FOR-EYE)
        (play-loop NASTY-3 NASTY-3 COMBINED-EGAL-EYE)
        (play-loop NASTY-3 PATSY-3 PATSY-3)
        (play-loop NASTY-3 SPASTIC-3 SPASTIC-3)
        (play-loop NASTY-3 TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE)
        (play-loop NASTY-3 SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE)
        (play-loop NASTY-3 COMBINED-EGAL-EYE COMBINED-EGAL-EYE)))

(define spastic-all-permutations
  (list (play-loop SPASTIC-3 NASTY-3 PATSY-3)
        (play-loop SPASTIC-3 NASTY-3 TOUGH-EYE-FOR-EYE)
        (play-loop SPASTIC-3 NASTY-3 SOFT-EYE-FOR-EYE)
        (play-loop SPASTIC-3 NASTY-3 COMBINED-EGAL-EYE)
        (play-loop SPASTIC-3 PATSY-3 TOUGH-EYE-FOR-EYE)
        (play-loop SPASTIC-3 PATSY-3 SOFT-EYE-FOR-EYE)
        (play-loop SPASTIC-3 PATSY-3 COMBINED-EGAL-EYE)
        (play-loop SPASTIC-3 TOUGH-EYE-FOR-EYE SOFT-EYE-FOR-EYE)
        (play-loop SPASTIC-3 TOUGH-EYE-FOR-EYE COMBINED-EGAL-EYE)
        (play-loop SPASTIC-3 SOFT-EYE-FOR-EYE COMBINED-EGAL-EYE)
        (play-loop SPASTIC-3 SPASTIC-3 SPASTIC-3)
        (play-loop SPASTIC-3 SPASTIC-3 NASTY-3)
        (play-loop SPASTIC-3 SPASTIC-3 PATSY-3)
        (play-loop SPASTIC-3 SPASTIC-3 TOUGH-EYE-FOR-EYE)
        (play-loop SPASTIC-3 SPASTIC-3 SOFT-EYE-FOR-EYE)
        (play-loop SPASTIC-3 SPASTIC-3 COMBINED-EGAL-EYE)
        (play-loop SPASTIC-3 PATSY-3 PATSY-3)
        (play-loop SPASTIC-3 NASTY-3 NASTY-3)
        (play-loop SPASTIC-3 TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE)
        (play-loop SPASTIC-3 SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE)
        (play-loop SPASTIC-3 COMBINED-EGAL-EYE COMBINED-EGAL-EYE)))

(define tough-all-permutations
  (list (play-loop TOUGH-EYE-FOR-EYE NASTY-3 PATSY-3)
        (play-loop TOUGH-EYE-FOR-EYE NASTY-3 SPASTIC-3)
        (play-loop TOUGH-EYE-FOR-EYE NASTY-3 SOFT-EYE-FOR-EYE)
        (play-loop TOUGH-EYE-FOR-EYE NASTY-3 COMBINED-EGAL-EYE)
        (play-loop TOUGH-EYE-FOR-EYE PATSY-3 SPASTIC-3)
        (play-loop TOUGH-EYE-FOR-EYE PATSY-3 SOFT-EYE-FOR-EYE)
        (play-loop TOUGH-EYE-FOR-EYE PATSY-3 COMBINED-EGAL-EYE)
        (play-loop TOUGH-EYE-FOR-EYE SPASTIC-3 SOFT-EYE-FOR-EYE)
        (play-loop TOUGH-EYE-FOR-EYE SPASTIC-3 COMBINED-EGAL-EYE)
        (play-loop TOUGH-EYE-FOR-EYE SOFT-EYE-FOR-EYE COMBINED-EGAL-EYE)
        (play-loop TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE)
        (play-loop TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE NASTY-3)
        (play-loop TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE SPASTIC-3)
        (play-loop TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE PATSY-3)
        (play-loop TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE SOFT-EYE-FOR-EYE)
        (play-loop TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE COMBINED-EGAL-EYE)
        (play-loop TOUGH-EYE-FOR-EYE PATSY-3 PATSY-3)
        (play-loop TOUGH-EYE-FOR-EYE NASTY-3 NASTY-3)
        (play-loop TOUGH-EYE-FOR-EYE SPASTIC-3 SPASTIC-3)
        (play-loop TOUGH-EYE-FOR-EYE SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE)
        (play-loop TOUGH-EYE-FOR-EYE COMBINED-EGAL-EYE COMBINED-EGAL-EYE)))

(define soft-all-permutations
  (list (play-loop SOFT-EYE-FOR-EYE NASTY-3 PATSY-3)
        (play-loop SOFT-EYE-FOR-EYE NASTY-3 SPASTIC-3)
        (play-loop SOFT-EYE-FOR-EYE NASTY-3 TOUGH-EYE-FOR-EYE)
        (play-loop SOFT-EYE-FOR-EYE NASTY-3 COMBINED-EGAL-EYE)
        (play-loop SOFT-EYE-FOR-EYE PATSY-3 SPASTIC-3)
        (play-loop SOFT-EYE-FOR-EYE PATSY-3 TOUGH-EYE-FOR-EYE)
        (play-loop SOFT-EYE-FOR-EYE PATSY-3 COMBINED-EGAL-EYE)
        (play-loop SOFT-EYE-FOR-EYE SPASTIC-3 TOUGH-EYE-FOR-EYE)
        (play-loop SOFT-EYE-FOR-EYE SPASTIC-3 COMBINED-EGAL-EYE)
        (play-loop SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE)
        (play-loop SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE NASTY-3)
        (play-loop SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE SPASTIC-3)
        (play-loop SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE PATSY-3)
        (play-loop SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE TOUGH-EYE-FOR-EYE)
        (play-loop SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE COMBINED-EGAL-EYE)
        (play-loop SOFT-EYE-FOR-EYE PATSY-3 PATSY-3)
        (play-loop SOFT-EYE-FOR-EYE NASTY-3 NASTY-3)
        (play-loop SOFT-EYE-FOR-EYE SPASTIC-3 SPASTIC-3)
        (play-loop SOFT-EYE-FOR-EYE TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE)
        (play-loop SOFT-EYE-FOR-EYE COMBINED-EGAL-EYE COMBINED-EGAL-EYE)))

(define combined-all-permutations
  (list (play-loop COMBINED-EGAL-EYE NASTY-3 PATSY-3)
        (play-loop COMBINED-EGAL-EYE NASTY-3 SPASTIC-3)
        (play-loop COMBINED-EGAL-EYE NASTY-3 TOUGH-EYE-FOR-EYE)
        (play-loop COMBINED-EGAL-EYE NASTY-3 SOFT-EYE-FOR-EYE)
        (play-loop COMBINED-EGAL-EYE PATSY-3 SPASTIC-3)
        (play-loop COMBINED-EGAL-EYE PATSY-3 TOUGH-EYE-FOR-EYE)
        (play-loop COMBINED-EGAL-EYE PATSY-3 SOFT-EYE-FOR-EYE)
        (play-loop COMBINED-EGAL-EYE SPASTIC-3 TOUGH-EYE-FOR-EYE)
        (play-loop COMBINED-EGAL-EYE SPASTIC-3 SOFT-EYE-FOR-EYE)
        (play-loop COMBINED-EGAL-EYE COMBINED-EGAL-EYE COMBINED-EGAL-EYE)
        (play-loop COMBINED-EGAL-EYE COMBINED-EGAL-EYE SOFT-EYE-FOR-EYE)
        (play-loop COMBINED-EGAL-EYE COMBINED-EGAL-EYE NASTY-3)
        (play-loop COMBINED-EGAL-EYE COMBINED-EGAL-EYE SPASTIC-3)
        (play-loop COMBINED-EGAL-EYE COMBINED-EGAL-EYE PATSY-3)
        (play-loop COMBINED-EGAL-EYE COMBINED-EGAL-EYE TOUGH-EYE-FOR-EYE)
        (play-loop COMBINED-EGAL-EYE PATSY-3 PATSY-3)
        (play-loop COMBINED-EGAL-EYE NASTY-3 NASTY-3)
        (play-loop COMBINED-EGAL-EYE SPASTIC-3 SPASTIC-3)
        (play-loop COMBINED-EGAL-EYE TOUGH-EYE-FOR-EYE TOUGH-EYE-FOR-EYE)
        (play-loop COMBINED-EGAL-EYE SOFT-EYE-FOR-EYE SOFT-EYE-FOR-EYE)))
        
|#


;; Problem 12

(define (history-summary cooperation mixed-behavior defection)
  (list cooperation mixed-behavior defection))

(define (cooperation-summary history-summary) (car history-summary))
(define (mixed-behavior-summary history-summary) (cadr history-summary))
(define (defection-summary history-summary) (caddr history-summary))

(define (cooperation-count partial-summary) (car partial-summary))
(define (defection-count partial-summary) (cadr partial-summary))
(define (total-count partial-summary) (caddr partial-summary))

(define (increase-c-count partial-summary)
  (list (+ (cooperation-count partial-summary) 1)
        (defection-count partial-summary)
        (+ (total-count partial-summary) 1)))

  (define (increase-d-count partial-summary)
    (list (cooperation-count partial-summary)
          (+ (defection-count partial-summary) 1)
          (+ (total-count partial-summary) 1)))

(define (increase-count choice partial-summary)
  (if (string=? choice "c")
      (increase-c-count partial-summary)
      (increase-d-count partial-summary)))

(define empty-partial-summary (list 0 0 0))
 

(define (make-history-summary hist0 hist1 hist2)
  (define (count-choices h0 h1 h2 cc cd dd)
    
    (define (update h0-play partial-summary)
      (cond ((eq? partial-summary 'cc)
             (count-choices (rest-of-plays h0) (rest-of-plays h1) (rest-of-plays h2)
                            (increase-count h0-play cc) cd dd))
            ((eq? partial-summary 'cd)
             (count-choices (rest-of-plays h0) (rest-of-plays h1) (rest-of-plays h2)
                            cc (increase-count h0-play cd) dd))
            ((eq? partial-summary 'dd)
             (count-choices (rest-of-plays h0) (rest-of-plays h1) (rest-of-plays h2)
                            cc cd (increase-count h0-play dd))))) 
    
    (if (empty-history? (rest-of-plays h1))
        (history-summary cc cd dd)
        (let ((opponent-moves (list (most-recent-play (rest-of-plays h1))
                                    (most-recent-play (rest-of-plays h2))))
              (player-response (most-recent-play h0)))
          (cond ((equal? opponent-moves '("c" "c")) (update player-response 'cc))           
                ((or (equal? opponent-moves '("c" "d")) (equal? opponent-moves '("d" "c")))
                 (update player-response 'cd))
                ((equal? opponent-moves '("d" "d")) (update player-response 'dd))))))
      
  (count-choices hist0 hist1 hist2
                 empty-partial-summary empty-partial-summary empty-partial-summary))

(define sample-summary
  (make-history-summary (list "c" "c" "d" "d" "c" "d" "c" "c")
                        (list "c" "c" "c" "d" "d" "c" "d" "c")
                        (list "c" "c" "d" "d" "d" "c" "c" "c")))
sample-summary


;; Problem 13
(define (get-probability-of-c summary)
  (map (lambda (scores)
         (let ((c-score (car scores)))
           (if (= c-score 0)
               '()
               (/ c-score (caddr scores)))))
       summary))

sample-summary
(get-probability-of-c sample-summary)


;; Problem 14

;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 

(define (test-entry expected-values actual-values) 
   (cond ((null? expected-values) (null? actual-values)) 
         ((null? actual-values) #f) 
         ;this part says that lists are commensurate when elements are the same or elements don't matter
         ((or (not (car expected-values)) ;first of expected values is #f, i.e. doesn't matter
              (not (car actual-values))   ;first of actual values is #f, i.e doesn't matter
              (= (car expected-values) (car actual-values))) ;values are equal
          (test-entry (cdr expected-values) (cdr actual-values))) 
         (else #f))) 

;; Checking if player0 is playing PATSY
(define (is-she-a-fool? hist0 hist1 hist2) 
   (test-entry (list 1 1 1)
               (get-probability-of-c 
                (make-history-summary hist0 hist1 hist2))))

;; Checking if player0 is playing PATSY, when there's not enough info to be sure
(define (could-she-be-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (map (lambda (x) 
                      (if (null? x) #f x))
                   (get-probability-of-c
                    (make-history-summary hist0 hist1 hist2)))))
                                                               
;; soft has probability of c (1 1 0)
(define (playing-soft-eye-for-eye? hist0 hist1 hist2)
  (test-entry (list 1 1 0)
              (get-probability-of-c
               (make-history-summary hist0 hist1 hist2))))

(define (could-be-playing-soft-eye-for-eye? hist0 hist1 hist2)
  (test-entry (list 1 1 0)
              (map (lambda (x)
                     (if (null? x) #f x)) ;test-entry threats #f as an unimportant value
                   (get-probability-of-c
               (make-history-summary hist0 hist1 hist2)))))

(define (DONT-TOLERATE-FOOLS my-history other1-history other2-history)
  (cond ((< (length my-history) 10) "c")
        ((and (could-she-be-a-fool? other1-history other2-history my-history)
              (could-she-be-a-fool? other2-history other2-history my-history))
         "d")
        (else "c")))