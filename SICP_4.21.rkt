#lang racket

;Exercise 4.21
;a
;Factorials
((lambda (n)
  ((lambda (fact) (fact fact n))
   (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
 5)


#|
((lambda (fact) (fact fact 10))
   (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))))

((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
 (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
 10)
(if (= 10 1) 1 (* 10 ((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
                      (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
                      9)))
...
which will lead to 
(* 10 (* 9 (* 8 (* 7 (* 6 (* 5 (* 4 (* 3 (* 2 (if (= 1 1) 1 ((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
                                                             (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
                                                             0)))))))))))
|#

;Fibonacci numbers
((lambda (n)
   ((lambda (f) (f f n))
    (lambda (fib k) (cond ((= k 0) 0) ((= k 1) 1) (else (+ (fib fib (- k 1)) (fib fib (- k 2))))))))
 3)


;b
(define (f x)
  (define (even? n)
    (if (= n 0) #t (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0) #f (even? (- n 1))))
  (even? x))

(define (foo x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1))))))

(f 52)
(foo 52)
                                            