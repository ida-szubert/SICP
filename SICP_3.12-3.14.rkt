#lang planet neil/sicp
;3.2 
;so how to evaluate procedures with assignment?
;environment- a sequence of frames
;frame- a table of bindings



;3.3
;modeling compound objects with changing state
;what is needed is designing data abstractions which contain not only constructors and selectors, but also mutators

;primitve mutators are set-car! and set-cdr!
;what about cons?
;we can writea procedure get-new-pair, which returns a pair that is not part of any existing list structure
;we get this new pair, then we set its car and cdr to desired values
;that might be an implementation of cons

;(define (cons x y)
;  (let ((new (get-new-pair)))
;    (set-car! new x)
;    (set-cdr! new y)
;    new))

;what get-new-pair actually is will be explained in section 5.3.1


;Exercise 3.12
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(define (append! l1 l2)
  (set-cdr! (last-pair l1) l2)
  l1)

(define (last-pair l)
  (if (null? (cdr l)) l (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
;z
;(a b c d)
;(cdr x)
;(b '())
;(define w (append! x y))
;w
;(a b c d)
;(cdr x)
;(b c d)

;the answers to (cdr x) differ, because in defining w we have changed x, so that now w and x are the same
;we no longer have acces to the original x
;and if we tried to call z once more, it would be different
;z
;strange, it's not

;Exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;(define v (make-cycle (list 'a 'b 'c)))
;v
;evaluating v never stops
;that's because the cdr of the last pair of v does not point to (), but to v itself
;so you can never get the value of v
;at first I was suprised because I thought that set-cdr! just takes the current value of x and sets it as the cdr, thus creating a new x, and it's done
;but the diagram of p.344 shows that set-cdr creates a pointer to the specified pair,
;rather than creating a new pair, identical to the specified one, and pointing to it instead of '()from the (cdr (last-pair x))

;Exercise 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;it is a function which reverses the list
;in the course of evaluation the original list gets truncated to its car only
(define v (list 'a 'b 'c 'd))
v
;(a b c d)
(define w (mystery v))
w
;(d c b a)
v
;(a)



