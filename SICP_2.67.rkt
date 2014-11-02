#lang racket

;Huffman Encoding Trees

;representing data as sequence of 0s and 1s
;e.g
;A 000
;B 001
;C 010
;D 011
;E 100
;F 101
;G 110
;H 111

;this is a fixed-length code, i.e. each symbol is represented with the same number of bits
;unlike e.g. More code
;when using a variable-length code the difficulty is in knowing you've reached the end of a symbol
;you can have a separator code
;or use a prefix code, i.e. such that no complete code for any symbol is a prefix of any other symbol's code
;it's economical to have a variable-length code to take advantage of the fact that some symbols occur much more frequantly than other
;e.g this is a prefix code:
;A 0
;B 100
;C 1010
;D 1011
;E 1100
;F 1101
;G 1110
;H 1111

;Huffman code:
;a binary tree
;leaves are the symbols; symbols have weights (corresponding to how often they are used)
;non-leaf nodes are sets containing all the symbols in the leaves below them
;nodes also have a weight, which is the sum of the weights of the leaves below

;if the code is represented by a tree, how do you find a symbol, knowing it's code and vice versa
;start at the root; every time you go left, add 0 to the symbol code, and if you go right, add 1
;how do we know down which branch to go?
;easy- you go and check- either you reach a leaf, and see if that's the symbol you're after
;or you reach a node and you'll check whether the symbol is among the ones this node dominates

;now, given an alphabet and relative weights, how to construct the best code (tree)

;find two leaves with the lowest weights and combine them
;iterate, with the newly constructed node in place of the two leaves
;this algorithm doesn't specify a unique tree

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right)) (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;the folowing procedure takes as input a list of pairs of segments and weights, e.g ((a 2) (b 2) (c 1) (d 4))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair)) (make-leaf-set (cdr pairs))))))

;Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;(A D A B B C A)

;Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-1 symbol current-branch)
    (cond ((and (leaf? (left-branch current-branch))
                (eq? (symbol-leaf (left-branch current-branch)) symbol))
           (list 0))
          ((and (leaf? (right-branch current-branch))
                (eq? (symbol-leaf (right-branch current-branch)) symbol))
           (list 1))
          ((member? symbol (left-branch current-branch)) (cons 0 (encode-symbol symbol (left-branch current-branch))))
          ((member? symbol (right-branch current-branch)) (cons 1 (encode-symbol symbol (right-branch current-branch))))
          (else (error "Symbol not present in the tree:" symbol))))
  (encode-1 symbol tree))


(define (member? symbol branch)
  (define (help symbol symbol-list)
  (cond  ((null? symbol-list) #f)
         ((eq? symbol (car symbol-list)) #t)
         (else (help symbol (cdr symbol-list)))))
  (help symbol (symbols branch)))

(encode '(A D A B B C A) sample-tree)
;(0 1 1 0 0 1 0 1 0 1 1 1 0)

;Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))) ;make-leaf-set creates an ordered list of leafes


(define (order-tree-leaf tree list)
  (if (null? list)
      (cons tree list)
      (if (<= (weight tree) (weight (car list)))
          (cons tree list)
          (cons (car list) (order-tree-leaf tree (cdr list))))))

(define (successive-merge leaf-list)
  (if (= (length leaf-list) 1)
      (car leaf-list)
      (successive-merge (order-tree-leaf (make-code-tree (car leaf-list) (cadr leaf-list)) (cddr leaf-list)))))

(define sample-pairs '((B 2) (D 1) (A 4) (C 1)))
(generate-huffman-tree sample-pairs)
;(define sample-leaves (make-leaf-set sample-pairs))
;sample-leaves
;(cddr sample-leaves)
;(define sub-tree (make-code-tree (car sample-leaves) (cadr sample-leaves)))
;sub-tree
;(weight sub-tree)
;(make-code-tree sub-tree (caddr sample-leaves))
          
                      
;Exercise 2.70

(define alphabet '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(make-leaf-set alphabet)
(define code-tree (generate-huffman-tree alphabet))
(define message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(encode message code-tree)
;(111101111111011110000000001111011111110111100000000011001101010101010101010111011000)
;85 bits
;were fixed-length code used, every symbol would have a 3 bit code
;the message uses 41 symbol occurences, therefore we would need 41*3 = 123 bits

;Exercise 2.71

(generate-huffman-tree '((a 1) (b 2) (c 4) (d 8) (e 16)))

;an alphabet of n symbols, with relative frequencies 1, 2, 4, ..., 2^(n-1)
;when we generate a Huffman tree for such an alphabet, it will always be exclusively left-branching
;we start with combining the two leafes with the smallest weight
;then we combine the newly created node with the next leaf
;and continue combining subsequent nodes with subsequent leaves, untill we reach the last leaf
;this is because 2^n + 2^(n+1) is always smaller than 2^(n+2)

;in a tree of this kind, the most frequent symbol will always be encoded with one bit
;because it is a daughter of the topmost node
;the two least frequent symbols will be encoded with (n-1) bits
;because we need as many bits as there were combining operations

;Exercise 2.72
;order of growth again
;Assuming that the relative frequencies of symbols are as in exercise 2.71
;assess the order of growth of the number of steps needed to encode the most frequent symbol:
;O(1)
;and the least frequent symbol:
;O(n)