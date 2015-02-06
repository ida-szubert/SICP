#lang racket
;Exercise 1.20
(define gcd
  (lambda (a b)
    (if (zero? b)
        a
        (gcd b (remainder a b)))))

;normal-orde evaluation: expand the expression fully before evaluating it

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

(if (= (remainder 206 40) 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

;evaluate one remainder
(if (= 6 0)
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(if (= (remainder 40 (remainder 206 40)) 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder
                                            (remainder 206 40)
                                            (remainder 40 (remainder 206 40)))))
;evaluate 2 nested remainders
(if (= 4 0)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder
                                            (remainder 206 40)
                                            (remainder 40 (remainder 206 40)))))

(if (= (remainder
        (remainder 206 40)
        (remainder 40 (remainder 206 40))) 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder
          (remainder 206 40)
          (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder
                                                                                         (remainder 206 40)
                                                                                         (remainder 40 (remainder 206 40))))))
;evaluate 4 nested remainders
(if (= 2 0)
    (remainder 40 (remainder 206 40))
    (gcd (remainder
          (remainder 206 40)
          (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40))
                                                        (remainder
                                                         (remainder 206 40)
                                                         (remainder 40 (remainder 206 40))))))
                                                                                         
                                                                                         
(if (= (remainder (remainder 40 (remainder 206 40))
                  (remainder (remainder 206 40)
                             (remainder 40 (remainder 206 40)))) 0)
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40)))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))

;evaluate 7 nested remainders
(if (= 0 0)
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40)))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))

;calculate remainders in the then-clause, i.e 4 remainders
;total of 18 calculations
          

;applicative-order evaluation: evaluate all the arguments beforehand
(gcd 206 40)
(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))

;one remainder calculated
(gcd 40 6)
(if (= 6 0)
    40
    (gcd 6 (remainder 40 6))) 

;one remainder calculated
(gcd 6 4)
(if (= 4 0)
    6
    (gcd 4 (remainder 6 4)))

;one remainder calculated
(gcd 4 2)
(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))

;one remainder calculated
(gcd 2 0)
(if (= 0 0)
    2
    (gcd 0 (remainder 2 0)))

;no more remainders calculated; total of 4 calculations