#lang planet neil/sicp

;Exercise 1.25

(define (square x) (* x x))

(define fast-expt
  (lambda (b n)
    (cond ((zero? n) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1)))))))

(define expmod
  (lambda (base exp m)
    (cond ((zero? exp) 1)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m)))))

(define expmod-AH
  (lambda (base exp m)
    (remainder (fast-expt base exp) m)))
;this is claimed to compute the exponential of a number modulo another number
;and it does

(expmod 3 4 4)
(expmod-AH 3 4 4)

;old version
(define fast-prime?
  (lambda (n times)
    (cond ((zero? times) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f))))

(define fermat-test
  (lambda (n)
    (define try-it
      (lambda (a)
        (= (expmod a n n) a)))
    (try-it (+ 1 (random (- n 1))))))

;AH version
(define fast-prime-AH?
  (lambda (n times)
    (cond ((zero? times) #t)
          ((fermat-test-AH n) (fast-prime-AH? n (- times 1)))
          (else #f))))

(define fermat-test-AH
  (lambda (n)
    (define try-it
      (lambda (a)
        (= (expmod-AH a n n) a)))
    (try-it (+ 1 (random (- n 1))))))

(fast-prime? 176 100)
(fast-prime-AH? 176 100)
;everything seems to work perfectly well

;old version
(define timed-prime-test
  (lambda (n)
    (newline)
    (display n)
    (start-prime-test n (runtime))))

(define start-prime-test
  (lambda (n start-time)
    (if (fast-prime? n 100)
        (report-prime (- (runtime) start-time)))))


;AH version
(define timed-prime-test-AH
  (lambda (n)
    (newline)
    (display n)
    (start-prime-test-AH n (runtime))))

(define start-prime-test-AH
  (lambda (n start-time)
    (if (fast-prime-AH? n 100)
        (report-prime (- (runtime) start-time)))))

(define report-prime
  (lambda (elapsed-time)
    (display " *** ")
    (display elapsed-time)))

;(timed-prime-test 100000007)
(timed-prime-test-AH 100000007)
;AH version does work, but it takes ages. Actually, execution never stops.
;and I don't yet know why