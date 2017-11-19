#lang htdp/asl
(require 2htdp/image)

;
; PROBLEM:
;
; In the Harry Potter movies, it is very important which of the four houses a
; wizard is placed in when they are at Hogwarts. This is so important that in
; most families multiple generations of wizards are all placed in the same family.
;
; Design a representation of wizard family trees that includes, for each wizard,
; their name, the house they were placed in at Hogwarts and their children. We
; encourage you to get real information for wizard families from:
;    http://harrypotter.wikia.com/wiki/Main_Page
;
; The reason we do this is that designing programs often involves collection
; domain information from a variety of sources and representing it in the program
; as constants of some form. So this problem illustrates a fairly common scenario.
;
; That said, for reasons having to do entirely with making things fit on the
; screen in later videos, we are going to use the following wizard family tree,
; in which wizards and houses both have 1 letter names. (Sigh)


(define-struct wiz (name house kids))
;; Wizard is (make-wiz String String (listof Wizard))
;; Interp: a Wizard with name, house and list of children.

(define Wa (make-wiz "A" "S" empty))
(define Wb (make-wiz "B" "G" empty))
(define Wc (make-wiz "C" "R" empty))
(define Wd (make-wiz "D" "H" empty))
(define We (make-wiz "E" "R" empty))
(define Wf (make-wiz "F" "R" (list Wb)))
(define Wg (make-wiz "G" "S" (list Wa)))
(define Wh (make-wiz "H" "S" (list Wc Wd)))
(define Wi (make-wiz "I" "H" empty))
(define Wj (make-wiz "J" "R" (list We Wf Wg)))
(define Wk (make-wiz "K" "G" (list Wh Wi Wj)))

(define IMG1-FAMILY-TREE (bitmap "../imgs/028-wizzard-family-tree-houses.jpg"))

; Template, arb-arity-tree, encapsulated with local.
(define (fn-for-wiz w)
  (local [(define (fn-for-wiz)
            (... (wiz-name w)
                 (wiz-house w)
                 (fn-for-low (wiz-kids w))))
          (define (fn-for-low low)
            (cond [(empty? low) (...)]
                  [else
                   (... (fn-for-wiz (car low))
                        (fn-for-low (cdr low)))]))]
    (fn-for-wiz w)))

;; I have to keep the parent's house in the lost context accumulator.
;; It is not that every children have to be in the same house as the
;; initial node to be listed, they have to be in the same house as their
;; immediate ancestor.
;;
;; Wizard E is not in the house G, but it is in the same house R as its
;; parent J, therefore, starting at Kg, E should be in the resulting list.
;; The same for F, which is also in the same house R as F's parent, and
;; A is in the same house as its parent G.

;; Wizard -> (listof String)
;; Produces a list of names of every wizard in the tree that was placed
;; in the same house as their immediate parent.
(check-expect (names We) empty)
(check-expect (names Wh) empty)
(check-expect (names Wg) (list "A"))
(check-expect (names Wk) (list "E" "F" "A"))

;; `ph` is an accumulator.

;; Template from Wizard plus lost context acc called ph (parent-house).
(define (names w)
  ;; ph is String;
  ;; The house of this wizard's immediate parent.
  ;; Root tree doesn't have a parent, so we just use "" for the ph.
  ;; (names Wk)
  ;; (fn-for-wiz Wk "")
  ;; (fn-for-wiz Wh "G")
  ;; (fn-for-wiz Wc "S")
  ;; (fn-for-wiz Wd "s")
  ;; (fn-for-wiz Wi "G")
  (local [;; Wizard String -> (listof String)
          (define (fn-for-wiz w ph)
            (if (string=? ph (wiz-house w))
                (cons (wiz-name w)
                      (fn-for-low (wiz-kids w) (wiz-house w)))
                (fn-for-low (wiz-kids w)
                            (wiz-house w))))

          ;; (listof Wizard) String -> (listof String)
          (define (fn-for-low low ph)
            (cond [(empty? low) empty]
                  [else
                   (append (fn-for-wiz (car low) ph)
                           (fn-for-low (cdr low) ph))]))]
    (fn-for-wiz w "")))

