#lang racket

;sets as binary trees

;each node holds one element of the set
;the branch to the left holds smaller elements
;the branch to the right holds bigger elements
;for each set there is a number of valid trees
;why use such a representation?
;if we want to check if x is a member of a set
;we compar x and the top node
;either they are equal, and the answer is #t
;or we know in which sub-tree should we continue to look for x
;from that follows that it's good to have balanced trees, where the top node is more or less the middle element of the set
;that way we halve the size of the problem each time we select a sub-tree

;an ordered set is like a very unbalanced tree, with the smallest element being the top node

;how to represent trees?
;each node is a list of three items- the entry at the node, the left sub-tree, and the right sub-tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
        ((> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))))

;it's difficult to be sure that the tree is balanced
;even if you start with a balanced one, adjoining may unbalance it
;it might be a good idea to have an operation to transform any tree into a balanced one
;so that once in a while we'll be able to balance out any possible biases

;Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree) (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

;a.
;do these procedures alway output the same result?
;it looks this way, since they do the same thing
;they both flatten the left and right branch by recoursive calls to tree->list
;and combine the flattened branches and the entry into one list

(define tree1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(tree->list-1 tree1)
(tree->list-2 tree1)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)

(define tree4 '(1 () (2 () (3 () (4 () (5 () (6 () (7 () ()))))))))
(tree->list-1 tree4)
(tree->list-2 tree4)

;the result is the same no matter which procedure or which set representation is used


;b.
;which procedure has a slower order of growth, if any?
;it seems that in both the number of steps depends linearly on the number of non-empty subtrees
;however, procedure one uses append, while procedure 2 uses cons to combine the node entries
;append is a recursive procedure repreatedly calling cons
;the number of steps of append depends on the number of elements of the first argument
;in procedure 1 the first argument in (left-branch tree), which contains more or less n/2 elements
;on each recursive call to procedure 1 the number of elements to append decreases by 2
;whereas procedure will perform, in the end, as many conses as there are elements

;therefore, procedure 1 has a slower order of growth

;Exercise 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
;tree is the first element of the result of applying partial-tree to the list x and the length of x

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

;a.

;left-size determines how many elements should the left sub-tree have; it's n-1 rather than n because we subtract the entry
;left-result is the result of applying partial-tree to the list of elements and the size of the left-branch
;left-tree is the the actual left sub-tree, i.e. the car of left-result; the rest of left-result is the list of unused elements
;we call them non-left-elts
;these are the elements of the right sub-tree and the entry
;right-size is the size of the right sub-tree, i.e. length of the whole list - size of the left sub-tree minus 1 (for entry)
;this-entry is the approximately middle element from the list; we define it as the first thing on the list of non-left elements
;right-result is the result of applying partial-tree to the list of the right sub-tree elements and the size of the right sub-tree
;right-tree is the actual right sub-tree
;and some elements may remain
;in the end, we make tree out of the entry (middle element), left sub-tree, and right sub-tree
;and cons it onto the remaining elements

;SO
;partial-tree divides the list of elements into two parts, roughly at the middle
;knowing the number of elements in the left sub-tree, it recoursively constructs this subtree from the list of elements, using up the specified number
;knowing the number of elements in the right sub-tree, it recoursively constructs this subtree from the elements leftover from the left subtree
;it makes a tree out of the middle element of the list of entries, the left sub-tree, and the right sub-tree

(list->tree '(1 3 5 7 9 11))

;b. 

;TODO; order of growth is really perplexing


;Exercise 2.65
;write union-set using tree->list and list->tree

(define (union-set set1 set2)
  (let ((list1 (tree->list-1 set1))
        (list2 (tree->list-1 set2)))
    (define (union-list l1 l2)
      (cond ((null? l1) l2)
            ((null? l2) l1)
            ((= (car l1) (car l2)) (cons (car l1) (union-list (cdr l1) (cdr l2))))
            ((< (car l1) (car l2)) (cons (car l1) (union-list (cdr l1) l2)))
            ((> (car l1) (car l2)) (cons (car l2) (union-list l1 (cdr l2))))))
    (list->tree (union-list list1 list2))))

(define (intersection-set set1 set2)
  (let ((list1 (tree->list-1 set1))
        (list2 (tree->list-1 set2)))
    (define (intersection-list l1 l2)
      (cond ((or (null? l1) (null? l2)) '())
            ((= (car l1) (car l2)) (cons (car l1) (intersection-list (cdr l1) (cdr l2))))
            ((< (car l1) (car l2)) (intersection-list (cdr l1) l2))
            ((> (car l1) (car l2)) (intersection-list l1 (cdr l2)))))
    (list->tree (intersection-list list1 list2))))



