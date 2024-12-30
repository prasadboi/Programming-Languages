#lang racket
(provide (struct-out bt-node) (struct-out bt-leaf) (struct-out bt-empty))
(provide (struct-out plus-node) (struct-out times-node) (struct-out int-leaf))
(provide palindrome-list eval-tree check-bt ml-split piles-insert)

; For each problem, you can find additional test cases and examples by looking in ps1-test.rkt.
; The general format of each test is of the form
;
;  (test-equal? "name" (f arg) expected)
;
; where f is a function you need to implement, arg is some test case arguments, and expected is what
; should be returned in that case. 
;
; If you don't understand what a problem is asking, look at the test cases for clarification.
; You can test your code by putting your ps1.rkt in the same directory as ps1-test.rkt and then
; running ps1-test.rkt in DrRacket or running "racket ps1-sol-test.rkt"
; from the command line.

; Submit your code by uploading the completed ps1.rkt to GradeScope.

;;;;;;;;; Problem 1 ;;;;;;;;;

; Recall that a palindrome or (palindromic word) is a word that is the
; same when read forwards and backwards.  For example, "kayak", "dad",
; and "radar", are all palindromes.

; Write a function palindrome-list which takes a string s as an argument
; and returns a list of all of the palindromic words that occur in s
; when s is converted to lower-case letters and all punctuation is
; removed.  The words should occur in the list in the order that they
; occur in the original string.  If a palindrome occurs multiple times,
; each occurrence should be in the list.

; Example: when s is "The gig was a gag, a joke.", (palindrome-list s) should return
; '("gig" "a" "gag" "a").

; define a function that inputs a string and tells you whether it is a palindrome or not
(define (reverse-str str)
  (list->string (reverse (string->list str))))

(define (not-punctuation c)
  (cond
    [(equal? #\, c) #f]
    [(equal? #\. c) #f]
    [(equal? #\: c) #f]
    [(equal? #\; c) #f]
    [(equal? #\' c) #f]
    [(equal? #\- c) #f]
    [(equal? #\" c) #f]
    [(equal? #\? c) #f]
    [(equal? #\! c) #f]
    [(equal? #\/ c) #f]
    [(equal? #\\ c) #f]
    [(equal? #\( c) #f]
    [(equal? #\) c) #f]
    [(equal? #\] c) #f]
    [(equal? #\[ c) #f]
    [else #t]
    )
  )

(define (remove-punctuation str)
  (list->string (filter not-punctuation (string->list str))))

(define (check-palindrome str)
  (cond
    [(equal?
      (string-downcase (remove-punctuation str))
      (string-downcase (remove-punctuation (reverse-str str))))
     (string-downcase (remove-punctuation str))
    ]
    ))

(define (palindrome-list s)
   (filter
    (lambda (x) (not (or (void? x) (equal? x ""))))
    (map check-palindrome (regexp-split #rx"[ \n\t,.:;\"?!()|]+" s)))
  )

;;;;;;;;; Problem 2 ;;;;;;;;;

; In this problem we consider trees that are constructed using
; plus-node, times-node, or int-leaf.  The arg1 and arg2 fields of
; plus-node and times-node should be themselves trees constructed from
; these constructors. The val field of int-leaf should be an integer.

; We shall think of these trees as representing arithmetic expressions,
; Where, for example, (int-leaf i) represents the number i, plus-node e1
; e2, represents e1 + e2, after interpreting the trees e1 and e2 as
; arithmetic expressions, and similarly for times-node e1 e2.
; 
; Write a function eval-tree, which takes a tree t as input and returns
; the integer that results from evaluating the arithmetic expression
; corresponding to the tree.  For example (eval-tree (plus-node
; (int-leaf 1) (int-leaf 2))) should return 3.


(struct plus-node (arg1 arg2))
(struct times-node (arg1 arg2))
(struct int-leaf (val))

;(define (eval-tree t) '())
(define (plus-node-op n)
  (
   cond
    [(and (int-leaf? (plus-node-arg1 n)) (int-leaf? (plus-node-arg2 n))) (+ (int-leaf-val (plus-node-arg1 n)) (int-leaf-val (plus-node-arg2 n)))]
    
    [(and (int-leaf? (plus-node-arg1 n)) (plus-node? (plus-node-arg2 n))) (+ (int-leaf-val (plus-node-arg1 n)) (plus-node-op (plus-node-arg2 n)))]
    [(and (int-leaf? (plus-node-arg1 n)) (times-node? (plus-node-arg2 n))) (+ (int-leaf-val (plus-node-arg1 n)) (times-node-op (plus-node-arg2 n)))]
    
    [(and (int-leaf? (plus-node-arg2 n)) (plus-node? (plus-node-arg1 n))) (+ (int-leaf-val (plus-node-arg2 n)) (plus-node-op (plus-node-arg1 n)))]
    [(and (int-leaf? (plus-node-arg2 n)) (times-node? (plus-node-arg1 n))) (+ (int-leaf-val (plus-node-arg2 n)) (times-node-op (plus-node-arg1 n)))]

    [(and (plus-node? (plus-node-arg1 n)) (plus-node? (plus-node-arg2 n))) (+ (plus-node-op (plus-node-arg1 n)) (plus-node-op (plus-node-arg2 n)))]
    [(and (plus-node? (plus-node-arg1 n)) (times-node? (plus-node-arg2 n))) (+ (plus-node-op (plus-node-arg1 n)) (times-node-op (plus-node-arg2 n)))]
    
    [(and (times-node? (plus-node-arg1 n)) (plus-node? (plus-node-arg2 n))) (+ (times-node-op (plus-node-arg1 n)) (plus-node-op (plus-node-arg2 n)))]
    [(and (times-node? (plus-node-arg1 n)) (times-node? (plus-node-arg2 n))) (+ (times-node-op (plus-node-arg1 n)) (times-node-op (plus-node-arg2 n)))]
  )
)

(define (times-node-op n)
  (
   cond
    [(and (int-leaf? (times-node-arg1 n)) (int-leaf? (times-node-arg2 n))) (* (int-leaf-val (times-node-arg1 n)) (int-leaf-val (times-node-arg2 n)))]
    
    [(and (int-leaf? (times-node-arg1 n)) (plus-node? (times-node-arg2 n))) (* (int-leaf-val (times-node-arg1 n)) (plus-node-op (times-node-arg2 n)))]
    [(and (int-leaf? (times-node-arg1 n)) (times-node? (times-node-arg2 n))) (* (int-leaf-val (times-node-arg1 n)) (times-node-op (times-node-arg2 n)))]
    
    [(and (int-leaf? (times-node-arg2 n)) (plus-node? (times-node-arg1 n))) (* (int-leaf-val (times-node-arg2 n)) (plus-node-op (times-node-arg1 n)))]
    [(and (int-leaf? (times-node-arg2 n)) (times-node? (times-node-arg1 n))) (* (int-leaf-val (times-node-arg2 n)) (times-node-op (times-node-arg1 n)))]

    [(and (plus-node? (times-node-arg1 n)) (plus-node? (times-node-arg2 n))) (* (plus-node-op (times-node-arg1 n)) (plus-node-op (times-node-arg2 n)))]
    [(and (plus-node? (times-node-arg1 n)) (times-node? (times-node-arg2 n))) (* (plus-node-op (times-node-arg1 n)) (times-node-op (times-node-arg2 n)))]
    
    [(and (times-node? (times-node-arg1 n)) (plus-node? (times-node-arg2 n))) (* (times-node-op (times-node-arg1 n)) (plus-node-op (times-node-arg2 n)))]
    [(and (times-node? (times-node-arg1 n)) (times-node? (times-node-arg2 n))) (* (times-node-op (times-node-arg1 n)) (times-node-op (times-node-arg2 n)))]
  )
)


(define (eval-tree t)
  (cond
    [(int-leaf? t) (int-leaf-val t)]
    [(plus-node? t) (plus-node-op t)]
    [(times-node? t) (times-node-op t)]
  )
)
;;;;;;;;; Problem 3 ;;;;;;;;;;;;

; In this problem we consider binary trees that are constructed using
; bt-node, bt-leaf, and bt-empty. int-leaf.

; The val fields of bt-node and bt-leaf are integers. The left and right
; fields of bt-node should be themselves trees constructed from these
; constructors.

; Recall that a binary search tree is a binary tree in which we have an
; invariant requiring that for a node of the form (bt-node i lt rt),
; every node value in the left child tree lt should be smaller than i,
; and every node in the right tree should be larger than i.

; Write a function check-bt which takes as an argument a binary tree t
; constructed using the above structs, and returns #t if t satisfies
; the binary tree invariant, and #f otherwise.

; Example: (check-bt (bt-node 5 (bt-leaf 1) (bt-leaf 6))) should return #t,
; but (check-bt (bt-node 5 (bt-leaf 6) (bt-leaf 6))) should return #f.

(struct bt-node (val left right))
(struct bt-leaf (val))
(struct bt-empty ())

(define (check-bst t min_t max_t)
  (
   cond
    [(bt-empty? t) #t]
    [(bt-leaf? t)
     (
      cond
      [(or (<= (bt-leaf-val t) min_t) (>= (bt-leaf-val t) max_t)) #f]
      [else #t]
     )
    ]

    [(bt-node? t)
     (
      cond
       [(or (<= (bt-node-val t) min_t) (>= (bt-node-val t) max_t)) #f]
       [else (and (check-bst (bt-node-left t) min_t (bt-node-val t)) (check-bst (bt-node-right t) (bt-node-val t) max_t))]
     )
    ]
  )
)

(define (check-bt t)
  (check-bst t -1e9 1e9)
)
;;;;;;;;; Problem 4 ;;;;;;;;;;;;

; Given a list of integers, '(i1 i2 ... ik), we say that the list is
; strictly monotone if either i1 < i2 < ... < ik or i1 > i2 > ... > ik.
; i.e. for either every element in the list is strictly smaller than the
; next element, or every element is strictly greater than the next
; element.
; 
; Write a function ml-split which takes a list l of integers and returns
; a list of lists obtained by breaking up l into strictly monotone
; lists. The returned lists should be maximal, meaning that there is no
; other splitting of l into strictly monotone lists in which any of the
; lists in the list could be larger while still being montone. In the
; case of ties, your solution should prefer to make the earlier lists
; larger.
; 
; Example: (ml-split '(1 2 3 4 3 2 1)) should return '((1 2 3 4) (3 2
; 1)).  Returning '((1 2) (3 4) (3 2 1)) would be wrong, because while
; each of the lists is strictly monotone, we could combine the first two
; to '(1 2 3 4), which is larger. Similarly, returning '((1 2 3) (4 3 2
; 1)) would be wrong because we should prefer to make the first list
; larger at the expense of making the second list shorter.



; function to append to a list


(define (monotone-list-helper l ctr id curr-monotone res)

  ;(newline)
  ;(print "| curr_monotone = ")
  ;(print curr-monotone)
  ;(print "| l = ")
  ;(print l)
  ;(print "| ctr = ")
  ;(print ctr)
  ;(print "| id = ")
  ;(print id)
  
  ; identifies the first monotone list in the given input
  (cond
    ; if empty do nothing, just append the curr-monotone to the res and terminate
    [(empty? l)
     (cond
       [(empty? curr-monotone) res]
       [else (reverse (cons curr-monotone (reverse res)))]
     )
    ]
    
    ; if list not empty
    ; if monotone list not started yet, just insert into monotone list and continue
    [(= ctr 0) (monotone-list-helper (cdr l) (+ 1 ctr) 0 (reverse (cons (car l) (reverse curr-monotone))) res)]
    ; if one element already in the list, check if the element smaller than the current element and use that to determine monotonicity 
    [(= ctr 1)
     (cond
     [(> (car l) (car curr-monotone)) ; increasing function 
      (monotone-list-helper (cdr l) (+ 1 ctr) 1 (reverse (cons (car l) (reverse curr-monotone))) res) 
     ]
     [(< (car l) (car curr-monotone)) ; decreasing function
      (monotone-list-helper (cdr l) (+ 1 ctr) -1 (reverse (cons (car l) (reverse curr-monotone))) res)
     ]
     [else (monotone-list-helper l 0 0 '() (reverse (cons curr-monotone (reverse res))))]
     )
    ]
    ; if 2 elements already in the list, then check if the current element holds the monotonicity or violates it. If violated then append the curr-monotone to res and start a new curr-monontone
    [(> ctr 1)
     ; check monotonicity
     (cond
       ; increasing
       [(= id 1)
        (
         cond
          ; if not violated
          [(> (car l) (car (reverse curr-monotone))) (monotone-list-helper (cdr l) (+ 1 ctr) id (reverse (cons (car l) (reverse curr-monotone))) res)]
          ; if violated
          [else (monotone-list-helper l 0 0 '() (reverse (cons curr-monotone (reverse res))))]
        )
       ]
       ; decreasing
       [(= id -1)
        (
         cond
          ; if not violated
          [(< (car l) (car (reverse curr-monotone))) (monotone-list-helper (cdr l) (+ 1 ctr) id (reverse (cons (car l) (reverse curr-monotone))) res)]
          ; if violated
          [else (monotone-list-helper l 0 0 '() (reverse (cons curr-monotone (reverse res))))]
        )
       ]
     )
    ]
  )
)

(define (ml-split l)
  (monotone-list-helper l 0 0 '() '())
)

;;;;;;;;; Problem 5 ;;;;;;;;;;;;

; Write a function piles-insert which takes two arguments.  When running
; (piles-insert ls n), the first argument ls is a list of lists of
; integers. We call each list in ls a pile. You may assume that each
; pile is non-empty, and that each pile is sorted in ascending
; order. Finally, you may also assume that if ls is of the form '(p_1
; p_2 ... p_n), then the head of p_i is strictly less than the head of p_(i+1).
; 
; Evaluating (piles-insert ls n) should return a list of piles obtained from
; taking ls and inserting n so that either n has (1) been added to the
; head of the first pile in ls whose previous head is greater than or
; equal to n, or (2) if no such pile exists, then a new pile containing
; just n is added to the end of ls.

; Example: (piles-insert '((4) (5)) 3) should return '((3 4) (5))) and
; (piles-insert '((2) (6)) 4) should return '((2) (4 6)))

(define (piles-insert-helper ls n res)
  ;(newline)
  ;(print "| l = ")
  ;(print ls)
  ;(print n)
  
  (
   cond
    [(empty? ls) (reverse (cons (list n) (reverse res)))]
    [ (>= (first (car ls)) n)
      (append (reverse (cons (cons n (car ls)) (reverse res))) (cdr ls))
    ]
    [else
      (piles-insert-helper (cdr ls) n (reverse (cons (car ls) (reverse res))))
    ]
  )
)

(define (piles-insert ls n)
  (piles-insert-helper ls n '())
)

(define pi-test3 '((4) (5)))