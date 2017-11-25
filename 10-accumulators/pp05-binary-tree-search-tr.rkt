#lang htdp/isl


; Problem:
;
; Starting with the following data definition for a binary tree (not a binary search tree)
; design a tail-recursive function called contains? that consumes a key and a binary tree
; and produces true if the tree contains the key.
;


(define-struct node (k v l r))
;; BT is one of:
;;  - false
;;  - (make-node Integer String BT BT)
;; Interp. A binary tree, each node has a key, value and 2 children
(define BT1 #f)
(define BT2 (make-node 1 "a"
                       (make-node 6 "f"
                                  (make-node 4 "d" #f #f)
                                  #f)
                       (make-node 7 "g" #f #f)))
;
; Template fro Binary Tree
;
#;
(define (fn-for-bt bt)
  (cond [(false? t) (...)]
        [else
         (... (node-k bt)
              (node-v bt)
              (fn-for-bt (node-l bt))
              (fn-for-bt (node-r bt)))]))

;; Natural BT -> Bool
;; Produce #t if BT contains a node with key k; #f otherwise.
(check-expect (contains? 1 #f) #f)
(check-expect (contains? 3 BT2) #f)
(check-expect (contains? 1 BT2) #t)
(check-expect (contains? 7 BT2) #t)


;
; Puts right branchs on todo list.
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;
#;
(define (contains? k bt)
  ;; todo: (listof BT) - List of unvisited right branches.
  (local [(define (contains/one? bt todo)
            (cond [(false? bt) (contains/list? todo)]
                  [else
                   (if (= k (node-k bt))
                       #t
                       (contains/one? (node-l bt)
                                      (cons (node-r bt) todo)))]))
          (define (contains/list? todo)
            (cond [(empty? todo) #f]
                  [else
                   (contains/one? (car todo) (cdr todo))]))]
    (contains/one? bt empty)))

;
; Puts both branches on todo list.
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;

(define (contains? k bt)
  ;; todo: (listof BT) - List of unvisited left and right branches.
  (local [(define (contains/one? bt todo)
            (cond [(false? bt) (contains/list? todo)]
                  [else
                   (if (= k (node-k bt))
                       #t
                       (contains/list? (cons (node-l bt)
                                             (cons (node-r bt)
                                                   todo))))]))
          (define (contains/list? todo)
            (cond [(empty? todo) #f]
                  [else
                   (contains/one? (car todo) (cdr todo))]))]
    (contains/one? bt empty)))

;
; In the first version, we call `contains/one?` and add only right branch
; into the worklist. In the second version, we instead invoke `contains/list?`
; and add both left and right branches into the worklist `todo` accumulator.
;

