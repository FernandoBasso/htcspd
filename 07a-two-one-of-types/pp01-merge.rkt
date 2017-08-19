#lang htdp/bsl


; Problem:
;
; Design the function merge. It consumes two lists of numbers, which it assumes are
; each sorted in ascending order. It produces a single list of all the numbers,
; also sorted in ascending order.
;
; Your solution should explicitly show the cross product of type comments table,
; filled in with the values in each case. Your final function should have a cond
; with 3 cases. You can do this simplification using the cross product table by
; recognizing that there are subtly equal answers.
;
; Hint: Think carefully about the values of both lists. You might see a way to
; change a cell content so that 2 cells have the same value.

;; =====================================================
;; DATA DEFINITIONS

;; ListOfNatural is one of:
;; - empty
;; - (cons Natural ListOfNatural)
;; Interp: a list of natural numbers in ascending order.

(define LON0 '())
(define LON1 (list 1))
(define LON2 (list 3 9))
(define LON3 (list 3 5 8))

;; =====================================================
;; FUNCTION DEFINITIONS

;; CROS-PRODUCT TABLE:
;;
;;       LA →     '()                 (cons Natural ListOfNatural
;;
;;  LB             LA                  LA
;;   ↓
;;
;; '()             LB                  cons smaller of firsts onto natural recursion
;;                                     passing rest of list with smaller and the
;; (cons Natural ListOfNatural)        entire other list
;;

(check-expect (merge '() '()) '())
(check-expect (merge '() (list 3 5)) (list 3 5))
(check-expect (merge (list 2 4) '()) (list 2 4))

(check-expect (merge (list 1) (list 2)) (list 1 2))
(check-expect (merge (list 2) (list 1)) (list 1 2))

(check-expect (merge (list 2 4) (list 3 5)) (list 2 3 4 5))
(check-expect (merge (list 4 5) (list 2 3)) (list 2 3 4 5))

(define (merge la lb)
  (cond [(empty? lb) la]
        [(empty? la) lb]
        [else
         (if (< (first la) (first lb))
             (cons (first la) (merge (rest la) lb))
             (cons (first lb) (merge la (rest lb))))]))

