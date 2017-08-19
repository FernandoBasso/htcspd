#lang htdp/bsl

(define-struct node (k v l r))
;; BinaryTree is one of:
;; - false
;; - (make-node Natural String BinaryTree BinaryTree)
;; interp. a binary tree, each node has key, value, and l/r children
(define BT0 false)
(define BT1 (make-node 1 "a" false false))
(define BT4 (make-node 4 "d"
                       (make-node 2 "b"
                                  (make-node 1 "a" false false)
                                  (make-node 3 "c" false false))
                       (make-node 5 "e" false false)))
;; Path is one of:
;; - empty
;; (cons "L" Path)
;; (cons "R" Path)
;; interp. a sequence of left and right 'turns' down though a BinaryTree
;;         (list "L" "R" "R" means take the left child of the root, then
;;         the right child of that node, and the right child again.
;;         empty means you have arrived at the destination.
(define P1 empty)
(define P2 (list "L"))
(define P3 (list "R"))
(define P4 (list "L" "R"))

;;
;; Design the function has-path? that consumes a BinaryTree and a Path.
;; The function should produce true if following the path through the
;; tree leads to a node. If the path leads to false or runs into false
;; before reaching the end of the path, the function should produce false.
;;

;;
;;                 | false |      (make-node Nat Str BT BT)
;; ----------------|-------|--------------------------------------
;;       empty     | false |                true
;; ----------------|-------|--------------------------------------
;; (cons "L" Path) | false | (has-path? <left-child> (rest path))
;; ----------------|-------|--------------------------------------
;; (cons "R" Path) | false | (has-path? <right-child> (rest path))
;;

;; BinaryTree Path -> Boolean
;; Produce #t if following p through bt leads to a node; #f otherwise.
(check-expect (has-path? #f '()) #f)
(check-expect (has-path? #f P2) #f)
(check-expect (has-path? #f P3) #f)
(check-expect (has-path? BT1 '()) #t)
(check-expect (has-path? BT4 (list "L")) #t)
(check-expect (has-path? BT4 (list "R")) #t)
(check-expect (has-path? BT4 (list "L" "L")) #t)
(check-expect (has-path? BT4 (list "L" "L" "R")) #f)

;(define (has-path? bt p) #f) ;stub

(define (has-path? bt p)
  (cond [(false? bt) #f]
        [(empty? p) #t]
        [(string=? "L" (first p)) (has-path? (node-l bt) (rest p))]
        [(string=? "R" (first p)) (has-path? (node-r bt) (rest p))]))
