#lang htdp/bsl

;
; Invariants
; ----------
; at each level:
;  - all accounts in left sub-tree have account number
;    less than root;
;  - all accounts in right sub-tree have account number
;    greater than root;
;

;
; PROBLEM:
;
; Design a data definition to represent binary search trees.
;

(define-struct node (key val l r))
;; BST (Bianry Search Tree) is one of:
;;  - #false (base case)
;;  - (make-node Integer String BST BST) (self-reference)
;; Interp: #false means no BST, or empty BST
;;         - key is the node's key
;;         - val is the node's value
;;         - l is left sub-tree
;;         - r is right sub-tree
;;
;; INVARIANT
;; ---------
;; for a given node:
;;    - key is > than all keys in its l(eft) child
;;    - key is < than all keys in its r(ight) child
;;    - the same key never appears more than once in the tree
(define BST0 #false)

(define BST1 (make-node 1 "abc" #f #f))

(define BST4 (make-node 4 "dcj" #f (make-node 7 "ruf" #f #f)))

(define BST3 (make-node 3 "ilk" BST1 BST4))

(define BST42
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" #f #f) #f)
             (make-node 50 "dug" #f #f)))

(define BST10
  (make-node 10 "why" BST3 BST42))

#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)                  ; Integer
              (node-val t)                  ; String
              (fn-for-bst (node-l t))       ; BST
              (fn-for-bst (node-r t)))]))   ; BST

;; Template rules used
;;  - one of: 2 cases
;;  - atomic distinct: #false
;;  - compound: (make-node Integer String BST BST)
;;  - self-reference: (node-l t) has type BST
;;  - self-reference: (node-r t) has type BST


;; =============================================================================
;; FUNCTIONS

;
; PROBLEM:                                      ;
; Design a function that consumes a BST and produces the sum of all
; the keys in the BST.
;

;; BST -> Natural
;; Produce the sum of all of the keys in the tree.
;; #f
(check-expect (sum-keys BST0) 0)
(check-expect (sum-keys BST3) (+ 3 1 4 7))

;(define (sum-keys t) 0) ;stub

(define (sum-keys t)
  (cond [(false? t) 0]
        [else
         (+ (node-key t)
            (sum-keys (node-l t))
            (sum-keys (node-r t)))]))

