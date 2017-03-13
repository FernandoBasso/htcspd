#lang htdp/bsl
(require 2htdp/image)

;; CONSTANTS

(define TXT-SIZE 14)
(define TXT-COLOR "black")
(define VSPC (rectangle 1 10 "solid" "white"))
(define HSPC (rectangle 10 1 "solid" "white"))
(define KVS ":") ; key value separator
(define MTTREE (rectangle 20 10 "solid" "white"))

;
; Consider the following data definition for a binary search tree:
;

;; Data definitions:

(define-struct node (key val l r))
;; A BST (Binary Search Tree) is one of:
;;  - false
;;  - (make-node Integer String BST BST)
;; interp. false means no BST, or empty BST
;;         key is the node key
;;         val is the node val
;;         l and r are left and right subtrees
;; INVARIANT: for a given node:
;;     key is > all keys in its l(eft)  child
;;     key is < all keys in its r(ight) child
;;     the same key never appears twice in the tree

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))
#;
(define (fn-for-bst t)
  (cond [(false? t) (...)]
        [else
         (... (node-key t)    ;Integer
              (node-val t)    ;String
              (fn-for-bst (node-l t))
              (fn-for-bst (node-r t)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic-distinct: false
;;  - compound: (make-node Integer String BST BST)
;;  - self reference: (node-l t) has type BST
;;  - self reference: (node-r t) has type BST

;; Functions:

; PROBLEM:
;
; Design a function that consumes a bst and produces a SIMPLE
; rendering of that bst. Emphasis on SIMPLE. You might want to
; skip the lines for example.
;


;; BST -> Image
;; Produce a _simple_ rendering of the tree.
(check-expect (render-bst #f) MTTREE)

(check-expect (render-bst BST1) (above (text (string-append "1" KVS "abc")
                                             TXT-SIZE
                                             TXT-COLOR)
                                       VSPC
                                       (beside (render-bst #f)
                                               HSPC
                                               (render-bst #f))))

(check-expect (render-bst BST4) (above (text (string-append "4" KVS "dcj")
                                             TXT-SIZE
                                             TXT-COLOR)
                                       VSPC
                                       (beside (render-bst #f)
                                               HSPC
                                               (render-bst (make-node 7 "ruf" #f #f)))))

(check-expect (render-bst BST3) (above (text (string-append "3" KVS "ilk")
                                             TXT-SIZE
                                             TXT-COLOR)
                                       VSPC
                                       (beside (render-bst BST1)
                                               HSPC
                                               (render-bst BST4))))


;(define (render-bst t) (square 0 "solid" "white")) ;stub

(define (render-bst t)
  (cond [(false? t) MTTREE]
        [else
         (above (render-key-val (node-key t) (node-val t))
                VSPC
                (beside (render-bst (node-l t))
                        HSPC
                        (render-bst (node-r t))))]))

;; Render from left to right instead of top to bottom.
#;
(define (render-bst t)
  (cond [(false? t) MTTREE]
        [else
         (beside (render-key-val (node-key t) (node-val t))
                (above (render-bst (node-l t))
                        (render-bst (node-r t))))]))

;; Integer String -> Image
;; Produce key/value pair with separator.
(check-expect (render-key-val 1 "foo")
              (text (string-append "1" KVS "foo") TXT-SIZE TXT-COLOR))

(define (render-key-val k v)
  (text (string-append (number->string k) KVS v)
                      TXT-SIZE
                      TXT-COLOR))

