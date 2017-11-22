#lang htdp/asl
(require 2htdp/image)

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
#;
(define (fn-for-wiz w)
  (local [(define (fn-for-wiz w)
            (... (wiz-name w)
g                 (wiz-house w)
                 (fn-for-low (wiz-kids w))))
          (define (fn-for-low low)
            (cond [(empty? low) (...)]
                  [else
                   (... (fn-for-wiz (car low))
                        (fn-for-low (cdr low)))]))]
    (fn-for-wiz w)))


;
; PROBLEM:
;
; Design a new function definition for same-house-as-parent that is tail
; recursive. You will need a worklist accumulator.
;

;; Wizard -> (listof String)
;; Produces a list of names of every wizard in the tree that was placed
;; in the same house as their immediate parent.
(check-expect (names We) empty)
(check-expect (names Wh) empty)
(check-expect (names Wg) (list "A"))
(check-expect (names Wk) (list "E" "F" "A"))
;
; Template: from Wizard (arb-arity tree) wrapped in local
;           - added work-list accumulator for tail recursion;
;           - added result so far accumulator for tail recursion;
;           - added compound data definition for wish list entries;
; Each entry on our work-list accumulator includes:
;  - the wizard to visit, together with
;  - the wizard's parent house.
;

(define (names w)
  ;; `todo` is: (listof WLE); a work-list accumulator.
  ;; `rsf`  is: (listof String); a result so far accumulator.
  (local [
          ;; wle = worklist entry
          (define-struct wle (w ph))
          ;; WLE (worklist entry) is (make-wle Wizard String)
          ;; Interp: A worklist entry with the wizard to pass to-fn-for-wiz
          ;;         and that wizard's parent house.

          (define (fn-for-wiz todo w ph rsf)
            (fn-for-low (append (map (λ (k)
                                       (make-wle k (wiz-house w)))
                                     (wiz-kids w))
                                todo)
                        (if (string=? (wiz-house w) ph)
                            ;; produces reversed result
                            ;(cons (wiz-name w) rsf)
                            ;; Doing it this way produces result in the expected order.
                            (append rsf (list (wiz-name w)))
                            rsf)))
          (define (fn-for-low todo rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-wiz (cdr todo)
                               (wle-w  (car todo))
                               (wle-ph (car todo))
                               rsf)]))]
    (fn-for-wiz empty w "" empty)))

