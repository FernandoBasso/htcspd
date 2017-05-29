#lang htdp/bsl
(require 2htdp/image)

; PROBLEM:
;
; Given the following data definition for a binary search tree,
; design a function that consumes a bst and produces a SIMPLE
; rendering of that bst including lines between nodes and their
; subnodes.
;
; To help you get started, we've added some sketches below of
; one way you could include lines to a bst.


;; -------------------------------------------------------------------------------------------------
;; CONSTANTS

(define TEXT-SIZE  14)
(define TEXT-COLOR "BLACK")

(define RECT-WIDTH 20)
(define RECT-HEIGHT (* RECT-WIDTH 2))

(define KEY-VAL-SEPARATOR ":")

(define MTTREE (rectangle 20 1 "solid" "red"))

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
; .

(define BST0 false)
(define BST1 (make-node 1 "abc" false false))
(define BST7 (make-node 7 "ruf" false false))
(define BST4 (make-node 4 "dcj" false (make-node 7 "ruf" false false)))
(define BST3 (make-node 3 "ilk" BST1 BST4))
(define BST42
  (make-node 42 "ily"
             (make-node 27 "wit" (make-node 14 "olp" false false) false)
             (make-node 50 "dug" false false)))
(define BST10
  (make-node 10 "why" BST3 BST42))
(define BST100
  (make-node 100 "large" BST10 false))
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

;
; Here is a sketch of one way the lines could work. What
; this sketch does is allows us to see the structure of
; the functions pretty clearly. We'll have one helper for
; the key value image, and one helper to draw the lines.
; Each of those produces a rectangular image of course.
;
; .
;
; And here is a sketch of the helper that draws the lines:
; .
; where lw means width of left subtree image and
;       rw means width of right subtree image



;; -------------------------------------------------------------------------------------------------
;; MAIN FUNCTION
;; BST -> Image
;; Renders BST with lines connecting nodes.
;; ASSUME: BST is balanced.

(check-expect (draw-bst (make-node 10 "why"
                                   (make-node 3 "ilk" #f #f)
                                   (make-node 42 "ily" #f #f)))
              (above (node->image (make-node 10 "why" #f #f))
                     (make-lines (* RECT-WIDTH 4) (* RECT-WIDTH 4))
                     (beside (draw-bst (make-node 3 "ilk" #f #f))
                             (draw-bst (make-node 42 "ily" #f #f)))))

;(define (draw-bst t) MTTREE) ;stub

(define (draw-bst t)
  (cond [(false? t) (node->image t)]
        [else
         (above (node->image t)
                (make-lines (image-width (draw-bst (node-l t)))
                            (image-width (draw-bst (node-r t))))
                (beside
                 (draw-bst (node-l t))
                 (draw-bst (node-r t))))]))

;; -------------------------------------------------------------------------------------------------
;; HELPER: node->image

(check-expect (node->image #f)
              (rectangle RECT-HEIGHT RECT-WIDTH "outline" "white"))


(check-expect (node->image (make-node 10 "why" #f #f))
              (overlay (text (string-append "10" KEY-VAL-SEPARATOR "why") TEXT-SIZE TEXT-COLOR)
                       (rectangle RECT-HEIGHT RECT-WIDTH "outline" "white")))

;; Node -> Image
;; Produce key/val image from given node.

;(define (node->image n) MTTREE) ;stub

(define (node->image n)
  (if (false? n)
      (rectangle RECT-HEIGHT RECT-WIDTH "outline" "white")
      (overlay (text (string-append (number->string (node-key n)) KEY-VAL-SEPARATOR (node-val n))
                     TEXT-SIZE
                     TEXT-COLOR)
               (rectangle RECT-HEIGHT RECT-WIDTH "outline" "white"))))

;; -------------------------------------------------------------------------------------------------
;; HELPER: lines
;; Create left and right connect lines based on width of subtrees.

(check-expect (make-lines 30 60)
              (add-line
               (add-line
                (rectangle (+ 30 60) (/ (+ 30 60) 4) "outline" "white")
                (/ (+ 30 60) 2)  0  (/ 30 2) (/ (+ 30 60) 4) "blue")
               (/ (+ 30 60) 2) 0 (+ 30 (/ 60 2)) (/ (+ 30 60) 4) "red"))

; (define (make-lines lw rw) MTTREE) ;stub

(define (make-lines lw rw)
  (add-line
   (add-line
    (rectangle (+ lw rw) (/ (+ lw rw) 4) "outline" "white")
    (/ (+ lw rw) 2) 0 (/ lw 2) (/ (+ lw rw) 4) "blue")
   (/ (+ lw rw) 2) 0 (+ lw (/ rw 2))  (/ (+ lw rw) 4) "red"))





