#lang planet neil/sicp

(define sum
  (lambda (term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b)))))

;that's a general procedure for sumation
;a is the start term, b is the end term
;term is the function that we apply to numbers being added up
;e.g. we might want to add the squares of all numbers between a and b
;them term would be (lambda (a) (* a a))
;next is the function by which you get the next a to input to term and add up
;it might be simple (+ a 1), but might be also more complex

(define inc
  (lambda (n)
    (+ n 1)))

(define cube
  (lambda (n)
    (* n n n)))

(define sum-cube
  (lambda (a b)
    (sum cube a inc b)))

(sum-cube 1 10)

(define (identity x) x)

(define sum-integers
  (lambda (a b)
    (sum identity a inc b)))

(sum-integers 1 20)

(define pi-sum
  (lambda (a b)
    (define pi-term
      (lambda (x)
        (/ 1.0 (* x (+ x 2)))))
    (define pi-next
      (lambda (x)
        (+ x 4)))
    (sum pi-term a pi-next b)))

(* 8 (pi-sum 1 1000))

(define integral
  (lambda (f a b dx)
    (define (add-dx x) (+ dx x))
    (* (sum f (+ a (/ dx 2)) add-dx b) dx)))

(integral cube 0 1 0.01)


(define simpson-integral
  (lambda (f a b n)
    (define h 
      (/ (- b a) n))
    (define (y k)
      (f (+ a (* k h))))
    (define (simpson-term k)
      (* (cond ((or (zero? k) (= k n)) 1)
               ((even? k) 2)
               (else 4))
         (y k)))
    (* (/ h 3) (sum simpson-term 0 inc n))))

(simpson-integral cube 0 1 100)
;returns 1/4, the exact integral of cube between 0 and 1
  
  


