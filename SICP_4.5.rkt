#lang racket

;Exercise 4.5

(define (cond? exp) (tagged-list exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))



;Here are the changes

(define (cond=>? clause) (eq? (cadr clause) '=>))

(define (cond-actions clause)
  (if (cond=>? clause)
      (cddr clause)
      (cdr clause)))

;What happens when clause list is empty or the current clause begins with else doesn't change
;There is one change really
;and that's the consequent of the if-expression that's being created
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (if (cond=>? first)
                (make-if (cond-predicate first)
                         ((cond-action first) (cond-predicate first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

;          (make-if (cond-predicate first)
;                   (if (cond=>? first)
;                       ((cond-action first) (cond-predicate first))
;                       (sequence->exp (cond-actions first)))
;                   (sequence->exp (cond-actions first)))
;This is another version. I canot decide if it's better or worse the the one currently used


;I wrote that because I'm not sure what happens with else clauses in => notation
;here I assume that recepient is applied to #true
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond=>? first)
            (if (cond-else-clause? first)
                ((cond-action first) #true)
                (make-if (cond-predicate first)
                         ((cond-action first) (cond-predicate first))
                         (expand-clauses rest)))
            (if (cond-else-clause? first)
                (sequence->exp (cond-actions first))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))