#lang racket

;Disadvantages of introducing assignment
;the substitution model goes out of the window
;with assignment, you can no longer view procedures as computing mathematical functions
;without, as it was done in the first two chapters, you can. Hence functional programming

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 25))
(W 20)
(W 10)

;without using set! we can write a procedure that subtracts its input from a stated balance, but there'll be no accumulated effect
(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 25))
(D 20)
(D 10)

;Apart from stomping over the substitution model, value assignment causes problems for the notion of sameness
;if you call make-decrimenter twice with the same variable, and ask if the two resulting objects are the same, a reasonable answer is yes
;they ware created in exactly the same way, they behave the same way, and are mutually substitutable within larger computatations
;much like synonyms
;when we call make-simplified-withdraw twice with the same variable
;the objects will initially be the same, in that given the same input they will produce the same output
;but at any point futher on that is not guaranteed
;they are certainly not multually substitutable
;but, were W2 put through the same set of calls as W1, they would be the same. In special case, they would be the same if nothing ever happened to them
;after they were created
;they are potentially the same


;What's bad about imperative programming?

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))

(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))
;there's a subtelty you have to think about- the order of assignment
;was counter set before product, the result would be wrong, because product would use the new value of counter
;this issue never occurs in functional programming, becuase you always know the values of variables on a given run of procedure

