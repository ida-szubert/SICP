#lang planet neil/sicp

(define (square x)
  (* x x))

(define expmod
  (lambda (base exp m)
    (cond ((zero? exp) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m)))))

(define fermat-test
  (lambda (n a)
    (= (expmod a n n) a)))


(define fermat-prime-help?
  (lambda (n counter)
    (cond ((= counter (- n 1)) #t)
          ((fermat-test n counter) (fermat-test n (+ counter 1)))
          (else #f))))

(define fermat-prime?
  (lambda (n)
    (fermat-prime-help? n 2)))

(define (fermat-full n)
   (define (iter counter)
     (cond ((= counter 1) #t)
           ((not (fermat-test n counter)) #f)
           (else (iter (- counter 1)))))
   (iter (- n 1)))

(fermat-prime? 7)
(fermat-prime? 561)
(fermat-prime? 1105)
(fermat-prime? 1729)
(fermat-prime? 2465)
(fermat-prime? 2821)
(fermat-prime? 6601)

;all of these appear to be prime numbers under fermat's theorem