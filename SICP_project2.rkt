#lang racket
;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0) (list score0 score1))
          (else (let ((game (make-play (most-recent-play history0)
                                       (most-recent-play history1))))
                  (get-scores-helper (rest-of-plays history0)
                                     (rest-of-plays history1)
                                     (+ (get-player-points 0 game) score0)
                                     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

(define (extract-entry game matrix)
  (cond ((null? matrix) (error "No results listed for the game:" game))
        ((equal? game (caar matrix)) (car matrix))
        (else (extract-entry game (cdr matrix)))))

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

(define test-extraxt-entry
  (let ((a-play (make-play "c" "d")))
    extract-entry a-play *game-association-list*))


;; A sampler of strategies

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
#|
(define nasty-tests
  (list (play-loop NASTY NASTY)
        (play-loop NASTY PATSY)
        (play-loop NASTY SPASTIC)
        (play-loop NASTY EGALITARIAN)
        (play-loop NASTY EYE-FOR-EYE)))

(define patsy-tests
  (list (play-loop PATSY PATSY)
        (play-loop PATSY NASTY)
        (play-loop PATSY SPASTIC)
        (play-loop PATSY EGALITARIAN)
        (play-loop PATSY EYE-FOR-EYE)))

(define spastic-tests
  (list (play-loop SPASTIC SPASTIC) 
        (play-loop SPASTIC NASTY)
        (play-loop SPASTIC PATSY)
        (play-loop SPASTIC EGALITARIAN)
        (play-loop SPASTIC EYE-FOR-EYE)))

(define egalitarian-tests
  (list (play-loop EGALITARIAN EGALITARIAN)
        (play-loop EGALITARIAN NASTY)
        (play-loop EGALITARIAN PATSY)
        (play-loop EGALITARIAN SPASTIC)
        (play-loop EGALITARIAN EYE-FOR-EYE)))

(define eye-for-eye-tests
  (list (play-loop EYE-FOR-EYE EYE-FOR-EYE)
        (play-loop EYE-FOR-EYE NASTY)
        (play-loop EYE-FOR-EYE PATSY)
        (play-loop EYE-FOR-EYE SPASTIC)
        (play-loop EYE-FOR-EYE EGALITARIAN)))
        
|#
#|           ---nasty---  ---patsy---  --spastic--  egalitarian  eye-for-eye

---nasty---      1             5          2.83           1            1

---patsy---      0             3          1.4            3            3

--spastic--     0.5            4          2.3           3.8          2.4

egalitarian      1             3          1.9            3            3

eye-for-eye      1             3          2.3            3            3

eye-for-2-eyes   1             3          1.8            3            3

eye-for-3-eyes   1             3          1.7            3            3
|#

;; 3 seems to be the optimal score - the result that maximizes the gains of both players
;; 5 is the highest score possible, but it is only attained by NASTY playing against PATSY
;; PATSY seems to be a strategy easy to beat, but in fact in 3/5 of configurations it leads to the optimal score of 3 for both players
;; NASTY on the other hand is the only strategy giving the maximum score, but overall it performs poorely, leaving both players with low scores
;; EGALITARIAN and EYE-FOR-EYE lead to similar results
;; they are in fact based on similar principles, egalitarian being sort of a delayed-judgement version of eye-for-eye


;; Problem 3
;; Here's an iterative version of the egalitarian strategy procedure 
(define (new-egalitarian my-history other-history)
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) "d" "c"))
          ((string=? (most-recent-play hist) "c") (majority-loop (+ cs 1) ds (rest-of-plays hist)))
          (else (majority-loop cs (+ ds 1) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history))
;; It is better in terms of space used
;; and it's better in terms of time
;; because instead of going through other-history twice, we only do it once


;; Problem 4
(define (EYE-FOR-TWO-EYES my-history other-history)
  (cond ((empty-history? my-history) "c")
        ((null? (rest-of-plays my-history)) (most-recent-play other-history))
        ((or (eq? (most-recent-play other-history) "c")
             (eq? (most-recent-play (rest-of-plays other-history)) "c")) "c")
        (else "d")))

(define eye-for-two-eyes-tests
  (list (play-loop EYE-FOR-TWO-EYES EYE-FOR-TWO-EYES)
        (play-loop EYE-FOR-TWO-EYES EYE-FOR-EYE)
        (play-loop EYE-FOR-TWO-EYES NASTY)
        (play-loop EYE-FOR-TWO-EYES PATSY)
        (play-loop EYE-FOR-TWO-EYES SPASTIC)
        (play-loop EYE-FOR-TWO-EYES EGALITARIAN)))

eye-for-two-eyes-tests
;; EYE-FOR-TWO-EYES is very much like EGALITARIAN and EYE-FOR-EYE
;; which should not come as a surprise given that conceptually it's in between


;;Problem 5
(define (make-eye-for-n-eyes n)
  (define (all-d? history)
    (cond ((null? history) #t)
          ((equal? (car history) "d") (all-d? (cdr history)))
          (else #f)))
  (define (n-most-recent history n)
    (if (= n 1)
        (cons (most-recent-play history) '())
        (cons (most-recent-play history)
              (n-most-recent (rest-of-plays history) (- n 1)))))
  (lambda (my-history other-history)
    (cond ((empty-history? my-history) "c")
          ((< (length my-history) n) "c")
          ((all-d? (n-most-recent other-history n)) "d")
          (else "c"))))
         
(define EYE-FOR-3-EYES (make-eye-for-n-eyes 3))
    
(define eye-for-3-eyes-tests
  (list (play-loop EYE-FOR-3-EYES EYE-FOR-3-EYES)
        (play-loop EYE-FOR-3-EYES EYE-FOR-TWO-EYES)
        (play-loop EYE-FOR-3-EYES EYE-FOR-EYE)
        (play-loop EYE-FOR-3-EYES NASTY)
        (play-loop EYE-FOR-3-EYES PATSY)
        (play-loop EYE-FOR-3-EYES SPASTIC)
        (play-loop EYE-FOR-3-EYES EGALITARIAN)))

eye-for-3-eyes-tests


;; Problem 6
(define (make-rotating-strategy strat0 strat1 freq0 freq1)     
  (lambda (my-history other-history)
    (if (< (remainder (length my-history) (+ freq0 freq1)) freq0)
        (strat0 my-history other-history)
        (strat1 my-history other-history))))


;; Problem 7
(define (make-higher-order-spastic strategies)
  (lambda (my-history other-history)
    ((list-ref strategies (remainder (length my-history) (length strategies)))
     my-history other-history)))

;; Problem 8
(define (gentle strat factor)
  (lambda (my-history other-history)
     (if (equal? (strat my-history other-history) "d")
         (if (= (random (/ 10 (* factor 10))) 0)
             "c"
             "d")
         (strat my-history other-history))))
  
