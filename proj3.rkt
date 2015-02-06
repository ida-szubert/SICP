#lang racket
(#%require (only racket/base sort))
(#%require (only racket/base require))

(require "the-web.rkt")

;;; SEARCH.SCM
;;; MIT 6.001                               Spring, 2005
;;; PROJECT 3


(define *search-debug* #f)         ; flag that shows search progress

;;; Searching and Indexing the World Wide Web.
;;;
;;; This file contains three major components, all of which are
;;; *not* web specific.  They are general purpose abstractions
;;; that we will then use to represent, search, and index the web.
;;;
;;;  1. Graph Abstraction -- directed graph with labeled nodes,
;;;                          node children (outgoing edges), and
;;;                          node contents
;;;
;;;  2. Search and        -- system to search a graph network looking
;;;     Search Strategy      for some goal
;;;
;;;  3. Index             -- an index associating a key with
;;;                          one or more values

;;;------------------------------------------------------------
;;; Graph Abstraction
;;;
;;;   Graph                     a collection of Graph-Elements
;;;   Graph-Element               a node, outgoing children from the
;;;                               node, and contents for the node
;;;   Node = symbol             a symbol label or name for the node
;;;   Contents = anytype        the contents for the node

;;---------------
;; Graph-Element

; make-graph-element: Node,list<Node>,Contents -> Element
(define (make-graph-element node children contents)
  (list 'graph-element node children contents))

(define (graph-element? element)            ; anytype -> boolean
  (and (pair? element) (eq? 'graph-element (car element))))

; Get the node (the name) from the Graph-Element
(define (graph-element->node element)       ; Graph-Element -> Node
  (if (not (graph-element? element))
      (error "object is not an element: " element)
      (first (cdr element))))

; Get the children (a list of outgoing node names)
; from the Graph-Element
(define (graph-element->children element)   ; Graph-Element -> list<Node>
  (if (not (graph-element? element))
      (error "object not element: " element)
      (second (cdr element))))

; Get the contents from the Graph-Element
(define (graph-element->contents element)   ; Graph-Element -> Contents
  (if (not (graph-element? element))
      (error "object not element: " element)
      (third (cdr element))))

(define (first list) (car list))
(define (second list) (cadr list))
(define (third list) (caddr list))
;;---------------
;; Graph

(define (make-graph elements)            ; list<Element> -> Graph
  (cons 'graph elements))

(define (graph? x)                  ; anytype -> boolean
  (and (pair? x) (eq? 'graph (car x))))

(define (graph-elements graph)           ; Graph -> list<Graph-Element>
  (if (not (graph? graph))
      (error "object is not a graph: " graph)
      (cdr graph)))

;apparently the first element listed in a graph is the root element
;but theoretically they could be listed in any order
;and a grah does not neccessarily have a root element, as far as I understand

(define (graph-root graph)		; Graph -> Node|null
  (let ((elements (graph-elements graph)))
    (if (null? elements)
        #f
        (graph-element->node (car elements)))))

; Find the specified node in the graph
(define (find-graph-element graph node)   ; Graph,Node -> Graph-Element|null
  (define (find elements)
    (cond ((null? elements) '())
          ((eq? (graph-element->node (car elements)) node)
           (car elements))
          (else (find (cdr elements)))))
  (find (graph-elements graph)))

; Find the children of the specified node in the graph
(define (find-node-children graph node)        ; Graph,Node -> list<Node>|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->children element)
        '()))) ;not perfect, '() is returned both when there are no children and when there is no node

; Find the contents of the specified node in the graph
(define (find-node-contents graph node)         ; Graph,Node -> Contents|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->contents element)
        '()))) ;same as with find-node-children

;; Testing...

(define test-graph
  (make-graph (list
   (make-graph-element 'a '(b i m) '(some words))
   (make-graph-element 'b '(c d e h) '(more words))
   (make-graph-element 'c '() '(at c node some words))
   (make-graph-element 'd '() '())
   (make-graph-element 'e '(f g) '(and even more words))
   (make-graph-element 'f '() '())
   (make-graph-element 'g '() '())
   (make-graph-element 'h '() '())
   (make-graph-element 'i '(j k l) '(more words yet))
   (make-graph-element 'j '() '())
   (make-graph-element 'k '() '())
   (make-graph-element 'l '() '()))))

(define test-cycle
  (make-graph (list
   (make-graph-element 'w '(u z x) '(words for node w))
   (make-graph-element 'u '(w) '(words for node u))
   (make-graph-element 'z '() '(words for node z))
   (make-graph-element 'x '(y v) '(words for node x))
   (make-graph-element 'v '(v) '(words for node v))
   (make-graph-element 'y '(z w) '(words for node y)))))

 ;(find-graph-element test-graph 'b)
 ;(find-graph-element test-graph 'z)
 ;(find-node-children test-graph 'b)
 ;(find-node-children test-graph 'z)
 ;(find-node-contents test-graph 'b)
 ;(find-node-contents test-graph 'z)


;;;------------------------------------------------------------
;;; Searching a network
;;;
;;; We define below a standard search procedure that walks
;;; over a graph in an effort to find a desired node.
;;; This version does not handle cycles in the graph

;; search: Node, (Node->Boolean), (Graph, Node -> List<Node>)
;;         (List<Node>, List<Node> -> List<Node>), Graph
;;           --> Boolean 

(define (search initial-state goal? successors merge graph)
  (define (search-inner still-to-do)
    (if (null? still-to-do)
        #f
        (let ((current (car still-to-do)))
          (when *search-debug*
              (begin (newline) (write 'now-at-) (write current))) ;so that we can see the search path if we set *search-debug* to #f
          (if (goal? current) ;and here's search proper
              #t
              (search-inner
               (merge (successors graph current) (cdr still-to-do)))))))
  (search-inner (list initial-state)))

(define (DFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append new old))
	  graph))


; (DFS-simple 'a
;             (lambda (node) (eq? node 'l))
;             test-graph)
  

;; you will need to write a similar search procedure that handles cycles


;;;------------------------------------------------------------
;;; Index Abstraction
;;;
;;;   An Index enables us to associate values with keys, and
;;; to retrieve those values later on given the key.
;;;
;;; Key = symbol
;;; Val = symbol

;; Index Implementation
;;
;;   An Index will be a tagged data object that holds a 
;; list of Index-Entries.  Each Index-Entry associates
;; a key with a list of values for that key, i.e.
;;   Index = Pair<Index-Tag, list<Index-Entry>>
;;   Index-Entry = list<Key, list<Val>>
;; 

(define (make-index-entry key value) (list key (list value)))
(define (get-value entry) (cadr entry))
(define (get-key entry) (car entry))

(define (make-index)            ; void -> Index
  (list 'index))

(define (index? x)          ; antype -> boolean
  (and (pair? x) (eq? 'index (car x))))

; An index can be reset to empty.
(define (reset-index! index)    ; Index -> Index
  (cond ((not (index? index))
         (error "object not an index: " index))
        (else (set! index (list 'index '()))
              index)))
      
; This is an internal helper procedure not to
; be used externally.
(define (find-entry-in-index index key)
  (if (not (index? index))
      (error "object not an index: " index)
      (let ((entry (assv key (cdr index))))
        (if entry entry '()))))


; returns a list of values associated with key
(define (find-in-index index key)       ; Index,Key -> list<Val>
  (let ((index-entry (find-entry-in-index index key)))
    (if (not (null? index-entry))
        (cadr index-entry)
        '())))


(define (add-to-index! index key value) ; Index,Key,Val -> Index
  (let ((entry (find-entry-in-index index key))
        (current-index-entries (cdr index)))
    (if (null? entry)
        (set! index (list 'index (cons (make-index-entry key value) current-index-entries)))
        (let ((current-values (get-value entry)))
          (set! entry (list key (list (add-if-not-present value current-values)))))))
  'ok)

(define (add-if-not-present element list)
  (if (member? element list)
      list
      (cons element list)))
     

;; Testing
 (define test-index (make-index))
 (add-to-index! test-index 'key1 'value1)
 (add-to-index! test-index 'key2 'value2)
(settest-index
 (find-in-index test-index 'key1)
; (add-to-index! test-index 'key1 'another-value1)
 
; (set-value! (find-entry-in-index test-index 'key2) 'value2&co)
; (find-in-index test-index 'key2)
; (find-in-index test-index 'key1)


;;------------------------------------------------------------
;; Finally, the Web!

;;--------------------
;; Web representation 
;;
;; We'll represent a "Web" as a graph.  Each Node in
;; the graph will be a URL; the node contents is the
;; Text inside the URL, and the node children is the
;; list of URL links inside the URL:
;;
;; Web = Graph
;; URL = Node
;; Text = list<Word>
;; Word = symbol      

; Procedures to get web links and web page contents:

(define (find-URL-links web url)
  (find-node-children web url))

(define (find-URL-text web url)
  (find-node-contents web url))



;-------------------------------------------------------------------------------------

;; Problem 1
(define (BFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append old new))
	  graph))

; (BFS-simple 'a
;             (lambda (node) (eq? node 'l))
;             test-graph)

;; Problem 2
(define (member? x list)
  (cond ((null? list) #f)
        ((eq? x (car list)) #t)
        (else (member? x (cdr list)))))

(define (search-with-cycles initial-state goal? successors merge graph)
  (define (search-inner still-to-do already-done)
    (cond ((null? still-to-do) #f)
          (else (let ((current (car still-to-do)))
                  (cond ((member? current already-done)
                         (search-inner (cdr still-to-do) already-done))
                        (else (when *search-debug*
                                  (begin (newline) (write 'now-at-) (write current)))
                              (if (goal? current)
                                  current
                                  (search-inner (merge (successors graph current) (cdr still-to-do))
                                                (cons current already-done)))))))))
  (search-inner (list initial-state) '()))

(define (append-if-new l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((member? (car l1) l2) (append-if-new (cdr l1) l2))
        (else (append-if-new (cdr l1) (cons (car l1) l2)))))

(define (DFS start goal? graph)
  (search-with-cycles start
                      goal?
                      find-node-children
                      (lambda (new old) (append-if-new new old))
                      graph))

;(DFS 'y (lambda (x) (eq? x 'v)) test-cycle)
;(DFS 'w (lambda (x) (eq? x 'y)) test-cycle)
;; w's firs child is u
;; if the search couldn't handle cycles, it would oscilate between w and u
;; (DFS-simple 'w (lambda (x) (eq? x 'y)) test-cycle)


(define (BFS start goal? graph)
  (search-with-cycles start
                      goal?
                      find-node-children
                      (lambda (new old) (append-if-new old new))
                      graph))

;(BFS 'w (lambda (x) (eq? x 'y)) test-cycle)

;; Searching the web

; (BFS 'http://sicp.csail.mit.edu/
;      (lambda (x) #f)
;      the-web)
; http://sicp.csail.mit.edu/
; http://sicp.csail.mit.edu/schemeimplementations
; http://sicp.csail.mit.edu/psets
; http://sicp.csail.mit.edu/getting-help.html
; *the-goal*
; http://sicp.csail.mit.edu/lab-use.html

; (DFS 'http://sicp.csail.mit.edu/
;     (lambda (x) #f)
;      the-web)
; http://sicp.csail.mit.edu/
; http://sicp.csail.mit.edu/schemeimplementations
; *the-goal*
; http://sicp.csail.mit.edu/lab-use.html
; http://sicp.csail.mit.edu/getting-help.html
; http://sicp.csail.mit.edu/psets


;; Problem 3

;; already defined
;(define (add-to-index! index key value) ; Index,Key,Val -> Index
;  (let ((entry (find-entry-in-index index key))
;        (current-index-entries (cdr index)))
;    (if (null? entry)
;        (set-cdr! index (cons (make-index-entry key value) current-index-entries))
;        (let ((current-value (get-value entry))) 
;          (cond ((null? current-value) (set-value! entry value))
;                ((not (equal? current-value value)) (set-value! entry
;                                                                (list current-value value)))))))
;  index)

;; Problem 4

;; add-document-to-index!: Index, Web, URL
 (define (add-document-to-index! index web url)
   (map (lambda (word) (add-to-index! index word url))
        (find-URL-text web url)))

 
(define the-web-index (make-index))
 
;(add-document-to-index! the-web-index
;                         the-web
;                         'http://sicp.csail.mit.edu/)
 
;(find-in-index the-web-index 'help)
;Value: http://sicp.csail.mit.edu/

;(find-in-index the-web-index '*magic*)
;; ;Value: '()

;(add-document-to-index! the-web-index
;                        the-web
;                        'http://sicp.csail.mit.edu/SchemeImplementations)

;(find-in-index the-web-index 'institute)


;; Problem 5
(define (crawl-and-do initial-state goal? action successors merge graph)
  (define (crawl-inner still-to-do already-done)
    (cond ((null? still-to-do) 'done)
          (else (let ((current (car still-to-do)))
                  (cond ((member? current already-done)
                         (crawl-inner (cdr still-to-do) already-done))
                        (else (when *search-debug*
                                  (begin (newline) (write 'now-at-) (write current)))
                              (action current)
                              (if (goal? current)
                                  #t
                                  (crawl-inner (merge (successors graph current) (cdr still-to-do))
                                                (cons current already-done)))))))))
  (crawl-inner (list initial-state) '()))

(define (BFC start action web)
  (crawl-and-do start
                (lambda (x) #f)
                action
                find-node-children
                (lambda (new old) (append-if-new old new))
                web))

(define (make-web-index web start)
  (define index (make-index))
  (BFC start
       (lambda (url) (add-document-to-index! index web url))
       web)
  (lambda (word) (find-in-index index word)))

(define find-documents (make-web-index the-web 'http://sicp.csail.mit.edu/))
(find-documents 'collaborative)
;('http://sicp.csail.mit.edu/psets 'http://sicp.csail.mit.edu/)

;; Problem 6
(define (search-any web start word)
  (BFS start
       (lambda (url)
         (if (member? word (find-URL-text web url))
             url
             #f))
       web))

(search-any the-web 'http://sicp.csail.mit.edu/getting-help.html 'collaborative)
;'http://sicp.csail.mit.edu/

(define (search-all web start word)
  (define result-list '())
  (BFS start
       (lambda (url)
         (if (member? word (find-URL-text web url))
             (begin (set! result-list (cons url result-list)) #f)
             #f))
       web)
  result-list)

(search-all the-web 'http://sicp.csail.mit.edu/getting-help.html 'collaborative)
;('http://sicp.csail.mit.edu/psets 'http://sicp.csail.mit.edu/)
       

;; Problem 7

(define web-3 (generate-random-web 3))
(define web-10 (generate-random-web 10))
(define web-50 (generate-random-web 50))
(define web-100 (generate-random-web 100))

(define find-documents-3 (make-web-index web-3 '*start*))
(define find-documents-10 (make-web-index web-10 '*start*))
(define find-documents-50 (make-web-index web-50 '*start*))
(define find-documents-100 (make-web-index web-100 '*start*))

(define (timed f . args)
  (let ((start (current-inexact-milliseconds)))
    (let ((val (apply f args)))
      (newline)
      (display "time expended: ")
      (display (- (current-inexact-milliseconds) start))
      (display " ms")
      val)))

; search any
(define test-search-any-help
  (list
(timed search-any web-3 '*start* 'help)      ;31 ms
(timed search-any web-10 '*start* 'help)     ;0 ms
(timed search-any web-50 '*start* 'help)     ;31 ms
(timed search-any web-100 '*start* 'help)))  ;109 ms

(define test-search-any-susan
  (list
(timed search-any web-3 '*start* 'Susanhockfield)     ;0 ms
(timed search-any web-10 '*start* 'Susanhockfield)    ;0 ms
(timed search-any web-50 '*start* ''Susanhockfield)   ;15 ms
(timed search-any web-100 '*start* 'Susanhockfield))) ;125 ms

(define test-search-all
  (list
(timed search-all web-3 '*start* 'help)      ;0 ms
(timed search-all web-10 '*start* 'help)     ;0 ms
(timed search-all web-50 '*start* 'help)     ;15 ms
(timed search-all web-100 '*start* 'help)))  ;141 ms

(define test-make-web-index
  (list
(timed make-web-index web-3 '*start*)        ;0 ms
(timed make-web-index web-10 '*start*)       ;0 ms
(timed make-web-index web-50 '*start*)       ;110 ms
(timed make-web-index web-100 '*start*)))    ;202 ms

(define test-find-help
  (list
(timed find-documents-3 'help)               ;0 ms
(timed find-documents-10 'help)              ;0 ms
(timed find-documents-50 'help)              ;0 ms
(timed find-documents-100 'help)))           ;0 ms

(define test-find-susan
  (list
(timed find-documents-3 'Susanhockfield)     ;0 ms
(timed find-documents-10 'Susanhockfield)    ;0 ms
(timed find-documents-50 'Susanhockfield)    ;0 ms
(timed find-documents-100 'Susanhockfield))) ;0 ms

test-search-any-help
test-search-any-susan
test-search-all
test-make-web-index
test-find-help
test-find-susan

;Indexing a web of size x definitelly takes longer than searching through it,
;but it's only done once (or incrementally, doesn't matter now)
;and each subsequent search takes next to no time.
;When providing a service, it's obviously better if the users don't have to wait


;; Problem 8
;More efficient indexing scheme
;Finding documents given a key amounts to listing all urls whose text includes the key
;So, we will optimize at the level of index rather then at the level of entry

;Optimized index stores entries in vecot form
;Vector has a fixed length, stores elements linearly, and can store and retrieve them from any point in constant time
;optimize-index takes an index and returns it with its entries alphabetically sorted

;(define (optimize-index index)
  
  
  
  
  