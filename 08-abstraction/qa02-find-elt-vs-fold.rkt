#lang htdp/isl

(define-struct elt (name data subs))
;; Element is (make-elt String Integer ListOfElement)
;; interp. An element in the file system, with name, and EITHER data or subs.
;;         If data is 0, then subs is considered to be list of sub elements.
;;         If data is not 0, then subs is ignored.

;; ListOfElement is one of:
;;  - empty
;;  - (cons Element ListOfElement)
;; interp. A list of file system Elements

(define F1 (make-elt "F1" 1 empty))
(define F2 (make-elt "F2" 2 empty))
(define F3 (make-elt "F3" 3 empty))
(define D4 (make-elt "D4" 0 (list F1 F2)))
(define D5 (make-elt "D5" 0 (list F3)))
(define D6 (make-elt "D6" 0 (list D4 D5)))


;                D6
;              /    \
;             /      \
;            D4      D5
;          /    \     |
;         /      \    |
;        F1      F2   F3


;
; PROBLEM
;
; If the tree is very large, then fold-element is not a good way to implement the
; find function from last week. Why? If you aren't sure then discover the answer
; by implementing find using fold-element and then step the two versions with
; different arguments.
;


;; (String Integer Y -> X) (X Y -> Y) Y Element -> X
;; The abstract fold/reduce function for Element.
;; https://groups.google.com/forum/#!topic/racket-users/K1mXIG8ECgs
(define (fold-elt combine-trees combine-lists b e)
  (local [
          ;; X -> Y
          (define (fn-for-elt e)  ; -> X
            (combine-trees (elt-name e)    ;String
                           (elt-data e)    ;Integer
                           (fn-for-loe (elt-subs e))))

          ;; Y -> Y
          (define (fn-for-loe loe)    ; -> Y
            (cond [(empty? loe) b]
                  [else
                   (combine-lists (fn-for-elt (first loe))
                                  (fn-for-loe (rest loe)))]))]
    (fn-for-elt e)))


;; String Element -> Integer or false
;; Search the given tree for an element with the given name,
;; produce data if found; false otherwise
(check-expect (find-with-fold "F3" F1) #f)
(check-expect (find-with-fold "F3" F3) 3)
(check-expect (find-with-fold "D4" D4) 0)
(check-expect (find-with-fold "D6" D6) 0)
(check-expect (find-with-fold "F3" D4) #f)
(check-expect (find-with-fold "F1" D4) 1)
(check-expect (find-with-fold "F2" D4) 2)
(check-expect (find-with-fold "F1" D6) 1)
(check-expect (find-with-fold "F3" D6) 3)


(define (find-with-fold n elt)
  (local [
          ;; String Integer (List-of Element) -> X
          (define (c1 name data loe)
            (if (string=? n name) data loe))

          ;; X -> Y -> Y
          (define (c2 x y)
                   (if (number? x)
                       x
                       y))]
    (fold-elt c1 c2 #f elt)))


;; String Element -> Integer or false
;; String ListOfElement -> Integer or false???
;; search the given tree for an element with the given name, produce
;; data if found; false otherwise
(check-expect (find "F3" F1) #f)
(check-expect (find "F3" F3) 3)
(check-expect (find "D4" D4) 0)
(check-expect (find "D6" D6) 0)
(check-expect (find "F3" D4) #f)
(check-expect (find "F1" D4) 1)
(check-expect (find "F2" D4) 2)
(check-expect (find "F1" D6) 1)
(check-expect (find "F3" D6) 3)


(define (find n e)
  (local [(define (find--elt n e)
            (if (string=? (elt-name e) n)
                (elt-data e)
                (find--loe n (elt-subs e))))

          (define (find--loe n loe)
            (cond [(empty? loe) false]
                  [else
                   (local [(define try (find--elt n (first loe)))]
                     (if (not (false? try))
                         try
                     (find--loe n (rest loe))))]))]

(find--elt n e)))

;; SOLUTION:

;
; In both cases the compiler optimizes the call to find-elt even though in the
; find-with-fold version we do not do that explicitly.
;
; Still, the fold version requires more function definitions and computations
; to be performed than the plain find version. For example, with
;
;     (check-expect (find "F3" F3) 3)
;
; we are done in step 17 while the version with fold we are still invoking c1/c2.
;
; In short, the plain version requires less steps than the version with fold,
; therefore being faster, which is crucial for long BSTs.
;

(time (find "F3" D6))
(time (find-with-fold "F3" D6))
; cpu time: 0 real time: 0 gc time: 0
; 3
; cpu time: 0 real time: 0 gc time: 0
; 3

